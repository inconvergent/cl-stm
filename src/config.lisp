(in-package #:evl)

(defvar *ctx* nil)

(define-condition evl-error (condition)
  ((expr :initarg :expr :reader expr)
   (msg :initarg :msg :reader msg))
  (:report (lambda (c s) (format s "██ expr:~%~s~%██ full msg:~%~a.~&" (expr c) (msg c))))
  (:documentation "EVL evaluation error for this expr w/msg.~&"))

(defun evl-error (expr msg)
  "raise evl-error condition."
  (error 'evl-error :expr expr :msg msg))

(defmacro evl/err/ctx (expr &body body)
  "evaluate body or raise evl-error condition"
  (with-gensyms (e)
     `(handler-case (progn ,@body)
        (error (,e) (evl-error ,expr ,e)))))

(defmacro evl/err/ctx-handle (expr &body body)
  (with-gensyms (e)
     `(handler-case (progn ,@body)
        (evl-error (,e) (warn "~%██████  EVL failed to evaluate:~%~s~%~a~&" ,expr ,e))
        (error (,e) (error "~%██████  EVL FATAL ERROR on:~%---~%~s~%---~%~a~&" ,expr ,e))
        )))

(defun preproc-env (a)
  (declare (list))
  (mapcar (lambda (x) (etypecase x (cons x) (symbol `(,x . ,x)))) a))

(defparameter +evl-dev-env+
  (preproc-env `(car?
                 env/empty env/new env/extend-alist env/extend-pair
                 dev/do-or dev/do-and
                 st/itr/1 st/itr/n st/itr/all
                 dev/eval-dsb dev/eval-mvb dev/eval-lambda dev/eval-coerce-values
                 dev/do-labels dev/do-let dev/do-cond))
  "convenient functions when implementing evl in evl.")

(defparameter +evl-math-env+
  (preproc-env
    `(+ - / * 1+ 1- t = < <= > >= evenp oddp
      abs min max signum floor round truncate float ceiling
      sqrt exp expt log mod rem gcd lcm sin cos tan asin acos atan sinh cosh tanh
      (pi . ,veq:fpi) (pii . ,veq:fpii) (pi5 . ,veq:fpi5)))
  "CL mathematical functions")

(defparameter +evl-cl-env+
  (preproc-env
    `(equal not zerop values values-list identity
      multiple-value-call (mvc . multiple-value-call) funcall mapcar mapc apply
      print princ format length subseq string= reverse list car cadr cdr cons
      cdar assoc pairlis acons first last second third nth intersection
      set-difference find find-if member union remove-if map mapcan every some
      append concatenate atom null stringp symbolp keywordp listp consp numberp
      functionp)))

(defparameter +evl-extra-env+
  (preproc-env
    `((atom? . atom) (null? . null) (even? . evenp) (odd? . oddp)
      (str? . stringp) (symbol? . symbolp) (keyword? . keywordp)
      (zero? . zerop) (some? . some) (every? . every)
      (list? . listp) (cons? . consp) (num? . numberp)
      (function? . functionp) (member? . member))))

(defparameter +std-env+
  `(,@+evl-dev-env+ ,@+evl-math-env+ ,@+evl-cl-env+ ,@+evl-extra-env+)
  "convenient standard environment (CL) functions and constant for evl.
none of them are required.")

