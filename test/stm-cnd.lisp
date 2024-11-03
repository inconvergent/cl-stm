(in-package #:stm-tests)

(plan 2)

(defun new-state (s &optional (cnt 0) (val 0))
  (fset:map (:state s) (:cnt cnt) (:val val)))

(defun inc-state (o s val)
  (new-state s (1+ (fset:@ o :cnt))
               (+ val (fset:@ o :val))))

(defun ping-list (o &rest rest)
  (declare (ignore rest))
  (list (fset:@ o :state) (fset:@ o :cnt) (fset:@ o :val)))

(subtest "test conditions"

  (stm:with-rules ((ping o (values o (stm:? pong (inc-state o :pong 1))))
                   (pong o (values o (stm:? ping (inc-state o :ping 3)))))

    (let ((sfx (stm:? ping (new-state :ping))))

      (labels ((val (o) (fset:@ o :val))
               (proc-item-10 (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 5)
                     (stm:cnd/halt-itr rule o :flag-10 "val is larger than 10" o))
                (ping-list o))
               (proc-item-20 (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 10)
                     (stm:cnd/discard-operation rule o :flag-20 "val is larger than 20"))
                 (ping-list o)))

        (is (mvb (a b c) (stm:itr/n sfx 30)
                 `(,(functionp a) ,(ping-list b) ,c))
            '(T (:PONG 29 57) NIL))

        (mvb (sfx val cnd) (stm:itr/n sfx 3 #'proc-item-10)
             (is (functionp sfx) t)
             (is val '(:PING 2 4))
             (is cnd nil))

        (mvb (sfx val cnd) (stm:itr/until sfx
                             (lambda (v) (equal v '(:PING 2 4)))
                             #'proc-item-10)
             (is (functionp sfx) t)
             (is val '(:PING 2 4))
             (is cnd nil))

        (mvb (sfx val cnd) (stm:itr/until sfx
                             (lambda (v) (equal v '(:FIZZ 2 4)))
                             #'proc-item-10)
             (is (functionp sfx) t)
             (is val '(:PONG 3 5))
             (is-type cnd 'stm:cnd/halt-itr))

        (mvb (sfx val cnd) (stm:itr/all sfx #'proc-item-10)
             (is (functionp sfx) t)
             (is val '(:PONG 3 5))
             (is-type cnd 'stm:cnd/halt-itr))

        (is-error (stm:itr/all sfx #'proc-item-20) 'stm:cnd/discard-operation)

        (mvb (sfx val cnd) (stm:itr/n sfx 30 #'proc-item-10)
             (is (functionp sfx) t)
             (is val '(:PONG 3 5))
             (is-type cnd 'stm:cnd/halt-itr))

        (stm:mvb (val cnd)
          (stm:mutate! (stm:acc/n sfx 30 #'proc-item-10))
          (is val '((:PONG 3 5) (:PING 2 4) (:PONG 1 1) (:PING 0 0)) )
          (is-type cnd 'stm:cnd/halt-itr)
          (is (stm:cnd/flag cnd) :flag-10))

        (stm:mvb (val cnd)
          (stm:mutate! (stm:acc/n sfx 30 #'proc-item-20))
          (is val nil)
          (is-type cnd 'stm:cnd/discard-operation)
          (is (stm:cnd/flag cnd) :flag-20))

        (stm:mvb (val cnd)
          (stm:mutate! (stm:acc/n sfx 10 #'ping-list))
          (is val '((:PONG 13 25) (:PING 12 24) (:PONG 11 21)
                    (:PING 10 20) (:PONG 9 17) (:PING 8 16)
                    (:PONG 7 13) (:PING 6 12) (:PONG 5 9) (:PING 4 8)))
          (is cnd nil))
          ))))

(subtest "test conditions 2"

  (stm:with-rules ((ping o (values o (stm:? pong (inc-state o :pong 1))))
                   (pong o (values o (stm:? ping (inc-state o :ping 3)))))

    (let ((sfx (stm:? ping (new-state :ping))))

      (labels ((val (o) (fset:@ o :val))
               (proc-stop (o rule)
                 (declare (keyword rule))
                 (if (> (val o) 5)
                     (stm:cnd/stop-itr rule o :stop))
                 (ping-list o)))
        (stm:mvb (val cnd)
          (stm:mutate! (stm:itr/n sfx 100 #'proc-stop))
          (is val '(:PONG 3 5) )
          (is-type cnd 'stm:cnd/stop-itr)
          (is (stm:cnd/flag cnd) :stop)
          (is sfx nil))))))

(unless (finalize) (error "error in test state conditions"))
