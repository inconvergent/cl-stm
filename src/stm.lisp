(in-package #:stm)

(declaim (inline r/identity r/print r/print* r/acc/val))
(declaim (inline itr/all itr/n itr/until))


(defmacro later (expr)
  "wrap expression in (lambda () ...) to evaluate later."
  `(lambda () ,expr))

(defmacro mutate! ((opr stx &rest rest) &body body)
  (declare (symbol stx) (null body))
  "perform operation and update stx to reference the next state.
unless the condition cnd/halt-operation is signalled.
returns: val/res, cnd
cnd can be cnd/halt-itr or cnd/halt-operation"
  (with-gensyms (nxt val e e*)
      ; e* is here bcs opr can be halted by cnd/halt-itr
     `(handler-case (mvb (,nxt ,val ,e*) (,opr ,stx ,@rest)
                         (setf ,stx ,nxt)
                         (values ,val ,e*))
        (cnd/halt-operation (,e) (values nil ,e)))))

(defmacro with-warnings ((val cnd) expr &body body)
  `(mvb (,val ,cnd) ,expr
     (typecase cnd
       (cnd/halt-itr (warn "halted itr w/~a @ {~a}: ~a"
                       (cnd/flag cnd) (cnd/rule cnd) (cnd/msg cnd)))
       (cnd/halt-operation (warn "halted mutation w/~a @ {~a}: ~a"
                            (cnd/flag cnd) (cnd/rule cnd) (cnd/msg cnd))))
     ,@body))

(defun r/identity (v rule)
  (declare (optimize speed) (ignore rule) (keyword rule))
  "default function for act / *act*."
  v)

(defun r/acc/val (val res) (declare (ignore res))
  "the accumulator used in all iterators."
  val)

(defun r/print (v rule)
  (declare (optimize speed) (ignore rule) (keyword rule))
  "print rule and value. return v."
  (format t "~&~a~&" v) v)

(defun r/print* (v rule)
  (declare (optimize speed) (keyword rule))
  "print rule and value. return v."
  (format t "~&; {~a}: ~a~&" rule v) v)

(declaim (function *act*))
(defvar *act* #'r/identity
  "function that is called for each iteration. requires
two arguments. the first argument is the value. must return the desired return
value for each iteration.
the second is the (keyword) name of the current rule.")


(defmacro stx/lambda ((name arg vfx) &body expr)
  (with-gensyms (v nxt act)
    `(lambda (&optional ,act) ; stx
       (let ((,arg (funcall ,vfx)))
         (multiple-value-bind (,v ,nxt) (progn ,@expr) ; expr returns v, nxt
           (cond ((and ,nxt (functionp ,nxt))
                  (values ,nxt #1=(funcall (the function (or ,act *act*)) ,v
                                           ,(lqn:kw! name))))
                 (,nxt (values nil #1#)) ; last value, no more stx
                 (t (values nil nil)))))))) ; fin

(defun make-rule-label (name arg expr)
  "create rule label with name, argument and rule/condition."
  (declare (symbol name arg))
  (with-gensyms (vfx)
    `((,name (,vfx)
       (declare (function ,vfx))
       (stx/lambda (,name ,arg ,vfx) ,expr)))))


(defmacro with-rules (rules &body body)
  (declare (list rules))
  "state machine context with rules/states.
ex:

  ; (with-rules
  ;   ((ping l (values l (new pong (list (1+ (cadr l)) :pong))))
  ;    (pong l (values l (new ping (list :ping (1+ (car l)))))))

  ;   (let* ((sm0 (new ping `(:ping 0)))  ; initial value. not evaluated here
  ;          (sm3 (itr/n sm0 3 #'princ))) ; eval & print 3 ping-pongs
  ;     (itr/n sm3 11 #'print)))          ; eval & print the next 11

see iterators and accumulators:
  - acc/all acc/n acc/until
  - itr/all itr/n itr/until

all iterators and accumulators use the act function to process each value
  before it is returned. the default is:

  ; (lambda (v rule) v) ; aka #'r/identity, which just returns the value.

  NOTE: also see r/print and r/print*, which are useful for development.
  NOTE: to override for the entire context set: stm:*act*.

all accumulators also have an acc and a res option:
  - acc is used for accumulation. the default is

    ; (lambda (v res) (cons v res)) ; aka. #'cons

    NOTE: you can filter which values are accumulated like this:

    ; (lambda (v rule res)
    ;   (if (my-testp rule v) (cons v res) res))

  - res is the initial value of the accumulation. default: (list).
    res does not have to be a list, but if you override you have to override
    acc to be compatible and vice-versa.
" `(labels (,@(mapcan (lambda (o) (apply #'make-rule-label o))
                      rules))
     ,@body))

(defmacro new (name expr)
  (declare (symbol name))
  "new state with this rule and expression. see with-rules."
  `(,name (later ,expr)))
(abbrev ? new)

(defun acc/all (stx &optional act (acc #'cons) res)
  (declare (optimize speed) (function stx acc))
  "accumulate all. see with-rules."
  (handler-case
    (mvb (nxt val) (funcall stx act)
      (declare (maybe-fuction nxt))
      (if nxt (acc/all nxt act acc (funcall acc val res))
              (values nil (funcall acc val res))))
    (cnd/halt-itr (cnd) (values stx res cnd))))

(defun acc/n (stx &optional (n 1) act (acc #'cons) res)
  (declare (optimize speed) (function stx acc) (fixnum n))
  "accumulate at most n times. see with-rules."
  (if (> n 0)
      (handler-case
        (mvb (nxt val) (funcall stx act)
          (declare (maybe-fuction nxt))
          (if nxt (acc/n nxt (1- n) act acc (funcall acc val res))
                  (values nil (funcall acc val res))))
        (cnd/halt-itr (cnd) (values stx res cnd)))
      (values stx res)))

(defun acc/until (stx &optional (until #'identity) act (acc #'cons) res)
  (declare (optimize speed) (function stx until acc))
  "accumulate until. see with-rules."
  (handler-case
    (mvb (nxt val) (funcall stx act)
      (declare (maybe-fuction nxt))
      (cond ((not nxt) (values nil (funcall acc val res)))
            ((not (funcall until val))
             (acc/until nxt until act acc (funcall acc val res)))
            (t (values nxt (funcall acc val res)))))
    (cnd/halt-itr (cnd) (values stx res cnd))))


(defun itr/all (stx &optional act res) ; thin wrapper
  (declare (optimize speed) (function stx))
  "iterate all. see with-rules."
  (acc/all stx act #'r/acc/val res))


(defun itr/n (stx &optional (n 1) act res) ; thin wrapper
  (declare (optimize speed) (function stx) (fixnum n))
  "iterate at most n times. see with-rules."
  (acc/n stx n act #'r/acc/val res))

(defun itr/until (stx &optional (until #'identity) act res) ; thin wrapper
  (declare (optimize speed) (function stx until))
  "iterate until. see with-rules."
  (acc/until stx until act #'r/acc/val res))


