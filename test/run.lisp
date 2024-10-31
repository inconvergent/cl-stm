
(setf prove:*enable-colors* nil)
(defpackage #:stm-tests
  (:use #:cl #:prove)
  (:import-from #:stm #:dsb #:mvb #:mvl)
  (:export #:run-tests))
(in-package #:stm-tests)

(defun -run-tests (files)
  (labels ((rel (f) (mapcar (lambda (p) (asdf:system-relative-pathname "stm/tests" p))
                            f)))
    (loop with fails = 0
          for f in (rel files)
          do ;(format t "~&~%starting tests in: ~a~%" (lqn:str! f))
             (unless (prove:run f :reporter :fiveam)
                     (incf fails))
             ;(format t "~&itrne: ~a~%" (lqn:str! f))
          finally (return (unless (< fails 1) (uiop:quit 7))))))

(defun run-tests ()
  (-run-tests '(#P"test/stm.lisp")))
