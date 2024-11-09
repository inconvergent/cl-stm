(load "~/quicklisp/setup.lisp")

(ql:quickload :stm)
(in-package :stm)


(defun new-state (s &optional (cnt 0) (val 0))
  (fset:map (:state s) (:cnt cnt) (:val val)))

(defun inc-state (o s val)
  (new-state s (1+ (fset:@ o :cnt))
               (+ val (fset:@ o :val))))

(defun main ()

  (with-rules ((ping (values $ (? pong (inc-state $ :pong 1))))
               (pong (values $ (? ping (inc-state $ :ping 3)))))

    (let ((sfx (? ping (new-state :ping))))

      (labels ((val (o) (fset:@ o :val))
               (proc-item-10 (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 10) ; halt when val is large
                     (cnd cnd/halt-itr rule :val-10 o "val is larger than 10" o)
                     (format t "~&; {~a cnd---10}: ~a~&" rule o)) ; print value
                 o)
               (proc-item-20 (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 20) ; discard operation when val is larger
                     (cnd cnd/discard-operation rule :val-20 o "val is larger than 20")
                     (format t "~&; {~a cnd-20}: ~a~&" rule o)) ; print value
                 o))

        (cnd/bind-warn (val cnd)
          (mutate! (itr/until sfx (lambda (o) (> (fset:@ o :val) 4)) #'proc-item-10))
          ; {PING cnd---10}: #{| (CNT 0) (VAL 0) (STATE PING) |}
          ; {PONG cnd---10}: #{| (CNT 1) (VAL 1) (STATE PONG) |}
          ; {PING cnd---10}: #{| (CNT 2) (VAL 4) (STATE PING) |}
          ; {PONG cnd---10}: #{| (CNT 3) (VAL 5) (STATE PONG) |}
          (print val)   ;  #{| (:CNT 3) (:VAL 5) (:STATE :PONG) |}
          (print cnd))  ;  nil ; we found something!

        (cnd/bind-warn (val cnd)
          (mutate! (itr/until sfx (lambda (o) (> (fset:@ o :val) 100)) #'proc-item-10))
          ; {PING cnd---10}: #{| (CNT 4) (VAL 8) (STATE PING) |}
          ; {PONG cnd---10}: #{| (CNT 5) (VAL 9) (STATE PONG) |}
          ; WARNING:
          ; ██ STM (PING) signalled CND/HALT-ITR w/ {VAL-10}
          ; ██   msg: val is larger than 10
          (print val)   ; #{| (:CNT 5) (:VAL 9) (:STATE :PONG) |}
          (print cnd))  ; #<CND/HALT-ITR {10033F7D13}>

        (cnd/bind-warn (val cnd)
          (mutate! (itr/n sfx 30 #'proc-item-20))
          ; {PING cnd-20}: #{| (CNT 6) (VAL 12) (STATE PING) |} ; starts at cnt = 6
          ; {PONG cnd-20}: #{| (CNT 7) (VAL 13) (STATE PONG) |}
          ; {PING cnd-20}: #{| (CNT 8) (VAL 16) (STATE PING) |}
          ; {PONG cnd-20}: #{| (CNT 9) (VAL 17) (STATE PONG) |}
          ; {PING cnd-20}: #{| (CNT 10) (VAL 20) (STATE PING) |}
          ; WARNING:
          ; ██ STM (PONG) signalled CND/DISCARD-OPERATION w/ {VAL-20}
          ; ██   msg: val is larger than 20
          (print val)   ; nil
          (print cnd) ) ; #<CND/DISCARD-OPERATION {1003404D43}>

        (cnd/bind-warn (val cnd)
          (mutate! (acc/n sfx 10))
          (print val)
          ; (#{| (:CNT 15) (:VAL 29) (:STATE :PONG) |} ; reversed, but starts at cnt=6 AGAIN
          ;  #{| (:CNT 14) (:VAL 28) (:STATE :PING) |}
          ;  #{| (:CNT 13) (:VAL 25) (:STATE :PONG) |}
          ; ...
          ;  #{| (:CNT 6) (:VAL 12) (:STATE :PING) |})
          )))))

(main)

