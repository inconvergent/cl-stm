
(in-package :stm)

(deftype maybe-fuction () "null or function" `(or function null))

(define-condition itr-halt (condition)
  ((state :initarg :state :reader state)
   (msg :initarg :msg :reader msg))
  ; (:report (lambda (c s) (format s "██ expr:~%~s~%██ full msg:~%~a.~&" (expr c) (msg c))))
  (:documentation "STM w/msg.~&"))

(defun itr-halt (expr msg)
  "raise itr-halt condition."
  (error 'itr-halt :expr expr :msg msg))

(defmacro err/ctx (expr &body body)
  "evaluate body or raise itr-halt condition"
  (with-gensyms (e)
     `(handler-case (progn ,@body)
        (error (,e) (itr-halt ,expr ,e)))))


(defmacro err/ctx-handle (expr &body body)
  (with-gensyms (e)
  `(handler-case (progn ,@body)
     (itr-halt (,e) (warn "aaaa: ~a///" ,e)))))
