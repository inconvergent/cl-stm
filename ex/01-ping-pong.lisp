(load "~/quicklisp/setup.lisp")

(ql:quickload :stm)
(in-package :stm)

; state machine with two states

(defun main ()

  (with-rules ((ping (values $ (? pong (list (1+ (cadr $)) :pong))))
               (pong (values $ (? ping (list :ping (1+ (car $)))))))

    (let* ((sm (? ping (list :ping 0)))
           (sm10 (itr/n sm 3 #'r/print*)))

      (lqn:out :----)
      (itr/n sm10 11 #'r/print*))))

(time (main))

