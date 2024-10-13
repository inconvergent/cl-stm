(in-package #:stm)

(declaim (function *act*))
; (defvar *act* #'(lambda (s) (lqn:out "~&(identity:~a)~&" s)))
(defvar *act* #'identity "override function used to process values. eg. #'print")
; (defvar _  nil)

(defmacro later (expr)
  "wrap expression in (lambda () ...) to evaluate later."
  `(lambda () ,expr))

(defun make-rule-label (rule-name arg rule-expr)
  "create rule label with name, argument and rule/condition."
  (declare (symbol rule-name arg))
  `((,rule-name (val-fx &aux (_ ,(lqn:kw! rule-name)))
       (declare (ignorable _) (function val-fx))
       (lambda (&optional act) ; make stx
         (multiple-value-bind (val nxt) ; current val, next stx
             (funcall (lambda (,arg) ,rule-expr) (funcall val-fx))
           (cond ((and nxt (functionp nxt))
                  (values nxt #1=(funcall (the function (or act *act*)) val)))
                 (nxt (values nil #1#)) ; last value, no more stx
                 (t (values nil nil)))))))) ; fin

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

all iterators and accumulators accept an act function of one argument which is
  called on each value before it is returned. default: #'identity.
  ; note: to override for the entire context set: evl:*act*.

all accumulators accept an acc and a res option.
  - acc is a function that accepts a value and an accumulated value, then returns
    the new accumualted value. default: #'cons.

    ; note: you can write your own function to filter out values. eg:
    ; (lambda (v res)
    ;    (if (my-testp v) (cons v res) res))

  - res is the initial value of the accumulation. default: (list).
    res does not have to be a list, but if you override you have to override
    acc to be compatible and vice-versa.
" `(labels (,@(mapcan (lambda (o) (apply #'make-rule-label o)) rules))
     ,@body))


(defmacro new (rule-name val-expr)
  (declare (symbol rule-name))
  "new state with this rule and expression. see with-rules."
  `(,rule-name (later ,val-expr)))
(defmacro ? (&rest rest) "alias for new." `(new ,@rest))


(defun acc/all (stx &optional (acc #'cons) act res)
  (declare (optimize speed) (function acc))
  "accumulate all. see with-rules."
  (if stx (multiple-value-bind (nxt val) (funcall (the function stx) act)
               (acc/all nxt acc act (funcall acc val res)))
          (values nil res)))

(defun acc/n (stx &optional (n 1) (acc #'cons) act res)
  (declare (optimize speed) (fixnum n) (function acc))
  "accumulate at most n times. see with-rules."
  (if (and stx (> n 0))
      (multiple-value-bind (nxt val) (funcall (the function stx) act)
           (acc/n nxt (1- n) acc act (funcall acc val res)))
      (values stx res)))

(defun acc/until (stx &optional (until #'identity) (acc #'cons) act res)
  (declare (optimize speed) (function until acc))
  "accumulate until. see with-rules."
  (if stx (multiple-value-bind (nxt val) (funcall (the function stx) act)
               (if (not (funcall until val))
                   (acc/until nxt until acc act (funcall acc val res))
                   (values stx (funcall acc val res))))
          (values nil res)))


(defun itr/all (stx &optional act res)
  (declare (optimize speed))
  "iterate all. see with-rules."
  (acc/all stx #'values act res))

(defun itr/n (stx &optional (n 1) act res)
  (declare (optimize speed) (fixnum n))
  "iterate at most n times. see with-rules."
  (acc/n stx n #'values act res))

(defun itr/until (stx &optional (until #'identity) act res)
  (declare (optimize speed) (function until))
  "iterate until. see with-rules."
  (acc/until stx until #'values act res))

