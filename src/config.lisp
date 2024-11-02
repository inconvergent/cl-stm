
(in-package :stm)

(deftype maybe-fuction () "null or function" `(or function null))

(define-condition cnd/base (condition)
  ((rule :initarg :rule :reader cnd/rule)
   (flag :initarg :flag :reader cnd/flag)
   (msg :initarg :msg :reader cnd/msg))
  (:documentation "STM base condition."))

; (:report (lambda (c s) (format s "~&██{HALT ITR @ ~a}: ~a~&" (rule c) (msg c))))
(define-condition cnd/halt-itr (cnd/base) nil
  (:documentation "halt ITR condition."))

(define-condition cnd/halt-operation (cnd/base) nil
  (:documentation "halt operation condition."))

(defun cnd/halt-itr (rule flag &rest msg)
  "halt itr/acc. return current value"
  (error 'cnd/halt-itr :rule rule  :flag flag
                       :msg (apply #'format nil msg)))

(defun cnd/halt-operation (rule flag &rest msg)
  "halt itr/acc mutation. return current value"
  (error 'cnd/halt-operation :rule rule  :flag flag
                             :msg (apply #'format nil msg)))

