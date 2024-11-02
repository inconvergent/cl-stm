(load "~/quicklisp/setup.lisp")

(ql:quickload :stm)
(in-package :stm)


(defun new-state (s &optional (cnt 0) (val 0))
  (fset:map (:state s) (:cnt cnt) (:val val)))

(defun inc-state (o s val)
  (new-state s (1+ (fset:@ o :cnt))
               (+ val (fset:@ o :val))))

(defun main ()

  (with-rules ((ping o (values o (? pong (inc-state o :pong 1))))
               (pong o (values o (? ping (inc-state o :ping 3)))))

    (let ((sfx (? ping (new-state :ping))))

      (labels ((val (o) (fset:@ o :val))
               (proc-item-10 (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 10) ; halt when val is large
                     (cnd/halt-itr rule :val-10 "val is larger than 10" o)
                     (format t "~&;---- {~a cnd-10}: ~a~&" rule o)) ; print value
                 o)
               (proc-item-20 (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 20) ; halt when val is larger
                     (cnd/halt-mutation rule :val-20 "val is larger than 20")
                     (format t "~&; {~a cnd-20}: ~a~&" rule o)) ; print value
                 o))

              ; act gets val as first value

        ; iteration is halted, but sfx is changed to the state where iteration
        ; was halted
        (with-warnings (val cnd)
          (mutate! (acc/n sfx 30 #'proc-item-10))
          (print val)
          )

        ; ; {PING cnd-10}: #{| (CNT 0) (VAL 0) (STATE PING) |}
        ; ; {PONG cnd-10}: #{| (CNT 1) (VAL 1) (STATE PONG) |}
        ; ; {PING cnd-10}: #{| (CNT 2) (VAL 4) (STATE PING) |}
        ; ; {PONG cnd-10}: #{| (CNT 3) (VAL 5) (STATE PONG) |}
        ; ; {PING cnd-10}: #{| (CNT 4) (VAL 8) (STATE PING) |}
        ; ; {PONG cnd-10}: #{| (CNT 5) (VAL 9) (STATE PONG) |}
        ; WARNING: halted itr @ {PING}: val is larger than 10

        ; restart where we stopped
        ; this prints output, but then mutation is halted and sfx remains
        ; unchanged
        (with-warnings (val cnd)
          (mutate! (acc/n sfx 30 #'proc-item-20))
          (print val)
          )

        ; ; {PING cnd-20}: #{| (CNT 6) (VAL 12) (STATE PING) |}
        ; ; {PONG cnd-20}: #{| (CNT 7) (VAL 13) (STATE PONG) |}
        ; ; {PING cnd-20}: #{| (CNT 8) (VAL 16) (STATE PING) |}
        ; ; {PONG cnd-20}: #{| (CNT 9) (VAL 17) (STATE PONG) |}
        ; ; {PING cnd-20}: #{| (CNT 10) (VAL 20) (STATE PING) |}
        ; WARNING: halted mutation @ {PONG}: val is larger than 20

        ; restart
        (with-warnings (val cnd)
          (mutate! (acc/n sfx 30 #'r/print*))
          (print val)
          )
        ; ; {PING}: #{| (CNT 6) (VAL 12) (STATE PING) |}
        ; ; {PONG}: #{| (CNT 7) (VAL 13) (STATE PONG) |}
        ; ; {PING}: #{| (CNT 8) (VAL 16) (STATE PING) |}
        ; ; {PONG}: #{| (CNT 9) (VAL 17) (STATE PONG) |}
        ; ; {PING}: #{| (CNT 10) (VAL 20) (STATE PING) |}
        ;; ...








          )))




  )
; )

(main)

