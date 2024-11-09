(in-package #:stm)

; TODO: override old
(defun /repl-arg (expr new &optional (old '$))
  ; (print (lqn:qry expr (?txpr (equal _ old) (progn new))))
  (subst new old expr))

(defmacro stx/lambda ((name vfx) &body expr)
  (with-gensyms (v nxt act arg)
    (let ((stx-name (lqn:sym! :stx/ name)))
      `(labels
         ((,stx-name (&optional ,act) ; stx
                     ; (declare (ftype (function (t)
                     ;                   (values t maybe-function)
                     ;                   ) aa ,stx-name))
              (let ((,arg (funcall ,vfx)))
                (multiple-value-bind (,v ,nxt) (progn ,@(/repl-arg expr arg)) ; expr returns v, nxt
                  ; (declare (maybe-function ,nxt)) ; not true
                  (cond ((functionp ,nxt)
                         (values ,nxt #1=(funcall (the function (or ,act *act*))
                                          ,v ,(lqn:kw! name))))
                        (,nxt (values nil #1#)) ; last value, no more stx
                        (t (values nil nil)))))))
         #',stx-name))))

(defun make-rule-label (name expr)
  "create rule label with name, argument and rule/condition."
  (declare (symbol name))
  (with-gensyms (vfx)
    `((,name (,vfx)
       (declare (function ,vfx))
       (stx/lambda (,name ,vfx) ,expr)))))


(defmacro with-rules (rules &body body)
  (declare (list rules))
  "
STATE MACHINE CONTEXT WITH RULES/STATES.

Here is an example that implements a state machine that flips between
  stats ping and pong, with corresponding behaviour.

ex:

  ; (with-rules
  ;   ((ping (values $ (? pong (list (1+ (cadr $)) :pong))))
  ;    (pong (values $ (? ping (list :ping (1+ (car $)))))))

  ;   (let* ((sm0 (? ping `(:ping 0)))  ; initial value. not evaluated here
  ;          (sm3 (itr/n sm0 3 #'princ))) ; eval & print 3 ping-pongs
  ;     (itr/n sm3 11 #'print)))          ; eval & print the next 11


  TODO: fix intro/description

a rule is defined as (name arg expr) where name is the name of the rule, arg is
  a symbol representing the current value and expr is an expression that
  must return
  (values current-value next-rule)

ITR / ACC - iterators and accumulators

  there are three accumulator / iterator functions:

  - acc/all acc/n acc/until
  - itr/all itr/n itr/until

  NOTE: the iterators are just thinwraps around the corresponding accumulator
        using the r/acc/val function. which just returns the value.

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

ACC/ITR & CONDITIONS

  iterators and accumulators have special handling of two conditions
  when signalled in eg act or acc functions:

  - cnd/halt-itr, halts iteration, returns values: val stx cnd;
  - cnd/stop-itr, halts iteration, returns values: val nil cnd.
" `(labels (,@(mapcan (lambda (o) (apply #'make-rule-label o))
                      rules))
     ,@body))

(defmacro new (name expr)
  (declare (symbol name))
  "new state with this rule and expression. see: with-rules."
  `(,name (later ,expr)))
(abbrev ? new)

(defmacro cnd/exec-stx ((stx act res) (nxt val) &body body)
  (declare (symbol stx act res nxt val))
  (with-gensyms (cnd)
    `(handler-case (mvb (,nxt ,val) (funcall ,stx ,act)
                        (declare (maybe-function ,nxt))
                        ,@body)
       (cnd/halt-itr (,cnd) (values ,stx ,res ,cnd))
       (cnd/stop-itr (,cnd) (values nil ,res ,cnd)))))

(declaim (inline itr/all itr/n itr/until)
  (ftype (function (function &optional t function t)
                   #1=(values maybe-function t maybe-cnd)) acc/all)
  (ftype (function (function &optional fixnum   t function t) #1#) acc/n)
  (ftype (function (function &optional function t function t) #1#) acc/until)

  (ftype (function (function &optional t t) #1#) itr/all)
  (ftype (function (function &optional fixnum t t) #1#) itr/n)
  (ftype (function (function &optional function t t) #1#) itr/until))

(defun acc/all (stx &optional act (acc #'cons) res)
  (declare (optimize speed))
  "accumulate all. see: with-rules."
  (cnd/exec-stx (stx act res) (nxt val)
    (if nxt (acc/all nxt act acc (funcall acc val res))
            (values nil (funcall acc val res) nil))))

(defun acc/n (stx &optional (n 1) act (acc #'cons) res)
  (declare (optimize speed))
  "accumulate at most n times. see: with-rules."
  (if (> n 0) (cnd/exec-stx (stx act res) (nxt val)
                (if nxt (acc/n nxt (1- n) act acc (funcall acc val res))
                        (values nil (funcall acc val res) nil)))
              (values stx res nil)))

(defun acc/until (stx &optional (until #'identity) act (acc #'cons) res)
  (declare (optimize speed))
  "accumulate until. see: with-rules."
  (cnd/exec-stx (stx act res) (nxt val)
    (cond ((not nxt) (values nil (funcall acc val res) nil))
          ((not (funcall until val))
           (acc/until nxt until act acc (funcall acc val res)))
          (t (values nxt (funcall acc val res) nil)))))


(defun itr/all (stx &optional act res) ; thinwrap
  (declare (optimize speed))
  "iterate all. see: with-rules."
  (acc/all stx act #'r/acc/val res))

(defun itr/n (stx &optional (n 1) act res) ; thinwrap
  (declare (optimize speed))
  "iterate at most n times. see: with-rules."
  (acc/n stx n act #'r/acc/val res))

(defun itr/until (stx &optional (until #'identity) act res) ; thinwrap
  (declare (optimize speed))
  "iterate until. see: with-rules."
  (acc/until stx until act #'r/acc/val res))

