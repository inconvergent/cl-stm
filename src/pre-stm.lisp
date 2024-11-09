(in-package #:stm)

(declaim (inline r/identity r/print r/print* r/acc/val))

(deftype maybe-function () "null or function" `(or function null))
(deftype maybe-keyword () "null or keyword" `(or keyword null))
(deftype maybe-cnd () "null or cnd/all" `(or cnd/all null))

(define-condition cnd/all (condition)
  ((ctx :initarg :ctx :reader cnd/ctx) (flag :initarg :flag :reader cnd/flag)
   (obj  :initarg :obj  :reader cnd/obj)  (msg  :initarg :msg  :reader cnd/msg))
  (:report (lambda (c s)
             (format s "~&██ STM (~a) signalled ~a w/ {~a}~%"
                       (cnd/ctx c) (type-of c) (cnd/flag c))
             (when (cnd/msg c) (format s "██   msg: ~a~&" (cnd/msg c)))))
  (:documentation "all conditions to control STM flow."))

(define-condition cnd/halt-itr (cnd/all) nil (:documentation "condition to halt ITR."))
(define-condition cnd/stop-itr (cnd/all) nil (:documentation "condition to stop ITR."))

(define-condition cnd/discard-operation (cnd/all) nil
  (:documentation "condition to discard operation."))

(defun msg-or-nil (msg)
  (when msg (apply #'format nil msg)))

(defmacro cnd (cnd ctx &optional (flag ctx) obj &rest msg)
  "signal any subtype of cnd/all w/metadata. use to halt/stop/discard any
  itr/acc/mutation operation. see: with-rules/ mutate!"
  `(signal ',cnd :ctx ,ctx :flag ,flag :obj ,obj :msg ,(msg-or-nil msg)))

(defmacro later (expr)
  "wrap expression in (lambda () ...) to evaluate later."
  `(lambda () ,expr))

(defmacro mutate! ((opr stx &rest rest) &body body)
  (declare (symbol stx) (null body))
  "perform operation and update stx to reference the next state.

ex:

 ; equal to mvb but with cnd warnings for cnd/all conditions
 (cnd/bind-warn (val cnd) ; result of itr/n, and conditions if any
                (mutate! (itr/n sfx 5))
   (print (list val cnd)))

CONDITIONS

  mutate! has special handling of cnd/discard-operation. it will handle the
  conditon by not mutating stx, and returning:

    values: nil cnd

  where cnd can be any cnd/all subcondition. eg. if the operation was halted.
  see with-rules for more details on conditions.
" (with-gensyms (nxt val e e*)
      ; e* is here bcs opr can be halted by cnd/halt-itr
     `(handler-case (mvb (,nxt ,val ,e*) (,opr ,stx ,@rest)
                         (setf ,stx ,nxt)
                         (values ,val ,e*))
        (cnd/discard-operation (,e) (values nil ,e)))))

(defmacro cnd/bind-warn ((val cnd) expr &body body)
  "(mv)bind val, cnd and warn if cnd is stm:cnd/all."
  `(mvb (,val ,cnd) ,expr
     (declare (maybe-cnd ,cnd))
     (typecase cnd (cnd/all (warn "~a" cnd)))
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

; TODO: update desc
(declaim (function *act*))
(defvar *act* #'r/identity
  "default function called for any operation that accepts an act argument. act is
  called for eachiteration.
  act requires two arguments. the first argument is the value.
the second is the (keyword) name of the current rule.")

