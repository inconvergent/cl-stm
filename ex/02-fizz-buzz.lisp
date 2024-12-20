(load "~/quicklisp/setup.lisp")

(ql:quickload :stm)
(in-package :stm)

; infinite fizzbuzz state machine

(defun which? (i)
  (list i (cond ((and #1=(zerop (mod i 3))
                      #2=(zerop (mod i 5))) :fizzbuzz)
                (#1# :fizz)
                (#2# :buzz)
                (t i))))

(defun fizz-print (s &rest rest)
  (typecase #4=(second s)
            (number (lqn:out " ~a" #4#))
            (t (lqn:out "~%~a" #4#))))

(defun main ()
  (with-rules ((fizzbuzz l
                     (values l (? fizzbuzz (which? (1+ (car l)))))))

    (let* ((sm (? fizzbuzz (which? 1)))
           (sm10 (itr/n sm 10 #'fizz-print)))

      (lqn:out :----)
      ; continue where we stopped
      (itr/n sm10 20 #'fizz-print)

      ; initiate an eternal state of fizzbuzz:
      ; (itr/all sm10 #'fizz-print)
      )))

(time (main))

