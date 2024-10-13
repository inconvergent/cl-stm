(in-package :evl)

; TODO: argument count guards
; TODO: optional symbol pass through
; TODO: &optional defaults does not work. see flat-arg-list

(defun env/empty () (env/new nil))

(defun env/new (&optional (a +std-env+))
  (declare (optimize (speed 3)) (list a))
  "create new environment (function) for EVL with this alist."
  (labels ((env/new/get-var (k &aux (res (assoc k a)))
             (declare (symbol k))
             (if res (cdr res)
                     (evl-error k (lqn:fmt "~&undefined variable: ~s~%" k)))))
    #'env/new/get-var))

; TODO: this is inefficient. we only have to check a if there is a hit in a
(defun env/merge (a b &aux (s (gensym)))
  (labels ((handle (fx k) (handler-case (funcall fx k) (evl-error (e) (declare (ignore e)) s)))
           (env/merge/get-var (k &aux (ra (handle a k))
                                      (rb (handle b k)))
             (cond ((and #1=(equal ra s) (equal rb s))
                    (evl-error k (lqn:fmt "~&undefined variable: ~s~%" k)))
                   (#1# rb) (t ra))))
    #'env/merge/get-var))

(defun env/extend-alist (a &optional (env (env/new)))
  (declare (optimize (speed 3)) (list a) (function env))
  "new env function extended with this alist."
  (labels ((env/extend/get-var (y &aux (res (assoc y a)))
             (declare (symbol y))
             (if res (cdr res) (funcall env y))))
    #'env/extend/get-var))

(defun env/extend-pair (kk vv &optional (env (env/new)))
  (declare (optimize (speed 3)) (function env) (list kk vv))
  "new env function extended with these names (kk) and values (vv)."
  (env/extend-alist (loop for k in kk and v in vv collect `(,k . ,v)) env))

(defun car? (l &rest ss)
  (declare (optimize speed) (list ss))
  "t if consp and car is a symbol in ss"
  (and (consp l) (member (car l) ss :test #'eq)))


(defun flat-arg-list (args)
  (declare (optimize (speed 3)) (list args))
  "flatten, and strip &-prefixed symbols."
  (remove-if (lambda (s) (match-pref (lqn:str! s) "&"))
             (flatten args)))

(defun dev/eval-dsb (args in expr evl* env*)
  (declare (optimize debug) (list args) (function evl* env*))
   "get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
(evl* '((lambda (,@args*) expr) ,@lst))
requires that evl* implements (quote ...) and ((lambda ...) ...)."
   (evl/err/ctx `(destructuring-bind ,args ,in ,@expr)
     (funcall evl* `((lambda ,#1=(flat-arg-list args) ,@expr)
                    ; quote the elements in the list. so they will not be  evaluated by evl*
                    ,@(mapcar (lambda (x) `(quote ,x))
                        ; use CL dsb to get variables as a list
                        (eval `(destructuring-bind ,args ',(funcall evl* in env*)
                                 (list ,@#1#)))))
             env*)))

(defun dev/eval-mvb (args in expr evl* env*)
  (declare (optimize debug) (list args) (function evl* env*))
   "get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
(evl* '((lambda (,@args*) expr) ,@lst))
requires that evl* implements (quote ...) and ((lambda ...) ...)."
  (evl/err/ctx `(mvb ,args ,in ,@expr)
    (funcall evl* `((lambda ,(flat-arg-list args) ,@expr)
                    ,@(mapcar (lambda (x) `(quote ,x))
                              (multiple-value-list (funcall evl* in env*))))
             env*)))

(defun dev/eval-lambda (args body evl* env*)
  (declare (optimize debug) (function evl* env*))
  "use CL eval to build a function with these args and body.
requires that evl* implements (progn ...)"
  (evl/err/ctx `(lambda (,@args) (progn ,@body))
    (eval `(lambda (,@args)
             (funcall ,evl* '(progn ,@body)
               (env/extend-pair ',#1=(flat-arg-list args)
                            (list ,@#1#) ,env*))))))

(defun dev/do-labels (pairs body evl* env*)
  (declare (optimize debug) (list body) (function evl* env*))
  "evaluate this body in an env with these labels (functions)."
  (labels ((wrp (k) (let ((res (find k pairs :key #'car)))
                               (if res (funcall evl* `(lambda ,@(cdr res)) #'wrp)
                                       (funcall env* k)))))
     (funcall evl* `(progn ,@body) #'wrp)))

(defun dev/do-let/coerce-list (vars)
  (mapcar (lambda (x) (etypecase x (list x) (symbol `(,x nil)))) vars))
(defun dev/do-let (vars body evl* env*)
  (declare (optimize debug) (list body) (function evl* env*))
  "evaluate body in an env with these named variables."
  (let ((vars (dev/do-let/coerce-list vars)))
    (funcall evl* `((lambda ,(mapcar #'car vars) (progn ,@body))
                  ,@(mapcar #'second vars))
                env*)))

(defun dev/do-cond (cnd x body evl* env*)
  (declare (optimize debug) (list body) (function evl* env*))
  "recursively evaluate these conds."
  (if (not x)
      (values (funcall evl* cnd env*)) ; only one value returned
      (funcall evl* `(if ,cnd ,x (cond ,@body)) env*)))

(defun dev/eval-coerce-values (expr evl* env*)
  (declare (optimize debug) (function evl* env*))
  "evaluate ~; coerce all values."
  (funcall evl* `(values ,@(mapcar (lambda (x) `(quote ,x))
                                   (mapcan (lambda (x) (multiple-value-list
                                                         (funcall evl* x env*)))
                                           expr)))
           env*))

(defun dev/eval-coerce-apply-values (fx expr evl* env*)
  (declare (optimize debug) (function evl* env*))
  "evaluate ~~; apply function to all coerce values."
  (multiple-value-call (funcall evl* fx env*)
    (dev/eval-coerce-values expr evl* env*)))

(defun dev/do-and (expr evl* env*)
  (declare (optimize debug) (list expr) (function evl* env*))
  "evaluate (and ...) expression with evl* in env*."
  (evl/err/ctx `(and ,@expr)
    (if expr (loop for e in expr for v = (multiple-value-list (funcall evl* e env*))
                   if (not (car v)) do (return-from dev/do-and nil)
                   finally (return-from dev/do-and (values-list v)))
             t)))

(defun dev/do-or (expr evl* env*)
  (declare (optimize debug) (list expr) (function evl* env*))
  "evaluate (or ...) expression with evl* in env*."
  (evl/err/ctx `(or ,@expr)
    (loop for e in expr for v = (multiple-value-list (funcall evl* e env*))
          if (car v) do (return-from dev/do-or (values-list v))
          finally (return-from dev/do-or nil))))

(defun evl* (expr env)
  (declare (optimize debug) (function env))
  "evaluate expr in env without error handling. see evl:evl for full docs."
  (let ((*ctx* expr))
  (cond ((null expr) expr)
        ((stringp expr) expr)
        ((numberp expr) expr)
        ((functionp expr) expr)
        ((keywordp expr) expr)
        ((symbolp expr) (funcall env expr)) ; get var from env

        ((car? expr 'declare) nil) ; ignore

        ((car? expr 'quote)
         (if (= (length expr) 2) (cadr expr)
                                 (evl-error expr "quote expects 1 argument")))

        ((car? expr 'cl-user::~ 'evl:~ 'veq:~) ; coerce value packs
         (dev/eval-coerce-values (cdr expr) #'evl* env))

        ((car? expr 'and) (dev/do-and (cdr expr) #'evl env))
        ((car? expr 'or)  (dev/do-or  (cdr expr) #'evl env))

        ((car? expr 'cl-user::~~ '~~) ; coerce apply fx to values
         (destructuring-bind (fx &rest rest) (cdr expr)
           (dev/eval-coerce-apply-values fx rest #'evl* env)))

        ((car? expr 'values)
         (values-list (mapcar (lambda (x) (evl* x env)) (cdr expr))))

        ((car? expr 'progn)
         (destructuring-bind (&optional a &rest rest) (cdr expr)
           (if rest (progn (evl* a env) (evl* (cons 'progn rest) env))
                    (evl* a env))))

        ((car? expr 'lambda 'lmb)
         (destructuring-bind (kk &rest rest) (cdr expr)
           (dev/eval-lambda kk rest #'evl* env)))

        ((car? expr 'let)
         (destructuring-bind (vars &rest body) (cdr expr)
           (dev/do-let vars body #'evl* env)))

        ((car? expr 'if)
         (destructuring-bind (test then &optional else) (cdr expr)
           (if (evl* test env) (evl* then env) (evl* else env))))

        ((car? expr 'when)
         (destructuring-bind (cnd &rest rest) (cdr expr)
          (evl* `(if ,cnd (progn ,@rest)) env)))

        ((car? expr 'unless)
         (destructuring-bind (cnd &rest rest) (cdr expr)
          (evl* `(if (not ,cnd) (progn ,@rest)) env)))

        ((car? expr 'cond)
         (evl/err/ctx `(,@expr)
           (when (cdr expr)
             (destructuring-bind ((cnd &optional x) &rest rest)
                  (cdr expr)
               (dev/do-cond cnd x rest #'evl* env)))))

        ((car? expr 'destructuring-bind 'dsb)
         (destructuring-bind (vars in &rest rest) (cdr expr)
           (dev/eval-dsb vars in rest #'evl* env)))

        ((car? expr 'multiple-value-bind 'mvb 'veq:mvb)
         (destructuring-bind (vars in &rest rest) (cdr expr)
           (dev/eval-mvb vars in rest #'evl* env)))

        ((car? expr 'multiple-value-list 'mvl)
         (multiple-value-list (evl* (cadr expr) env)))

        ((car? expr 'labels 'lbl)
         (destructuring-bind (pairs &rest body) (cdr expr)
           (dev/do-labels pairs body #'evl* env)))

        ((consp expr) ; (apply (fx/lambda) ...)
         (evl/err/ctx `(,@expr)
           (apply (evl* (car expr) env)
                  (mapcar (lambda (x) (evl* x env))
                          (cdr expr)))))
        (t (error "~&-->>~%invalid expression:~%  ~s <<--~&"
                  expr)))))


(defun evl (expr &optional (env (env/new)))
  (declare (optimize debug) (function env))
  "evaluate an EVL expression in env.

arguments:
 - expr: the expression that should be evaluated.
 - env: a funcion used to lookup a variable in scope. see: (evl:env/new)

supports CL syntax:
  - if, and, or, cond, when, unless, progn
  - lambda (lmb), labels (lbl),
  - let, quote, values, multiple-value-list (mvl),
  - destructuring-bind (dsb), multiple-value-bind (mvb),

non CL syntax:
  - (~ ...) coerce value packs to a single values.
  - (~~ fx ...) coerce all values and apply fx.

deviations from regular CL syntax:
  - there is no function name space; variables and functions in environment are
    indistinguishable.
  - inconsistent (left to right) argument evaluation (TODO: check).
  - &optional, &key and &rest are supported as arguments in lambda, labels,
    destructuring-bind. but default values in optional/key are not supported (yet),
    so all defaults are nil.
  - &aux is not supported.
  "
  (evl/err/ctx-handle expr (evl* expr env)))

(defmacro with-env ((&optional (env (env/new))) &body body)
  "evaluate '(progn ,@body) in env with error handling."
  (with-gensyms (env*)
    `(let ((,env* ,env)) (declare (function ,env*))
       (evl '(progn ,@body) ,env*))))

