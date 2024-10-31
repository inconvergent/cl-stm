
(in-package #:stm-tests)

(plan 3)

(defun r/princ (v r) (princ r) (princ v))

(subtest "test state machine"
   (stm:with-rules ((rulex i (cond ((< i 4)  (values i (stm:new rulex (1+ i)))))))
     (let ((stm:*act* (lambda (v r) (lqn:out "hi~a ~a|>" r v))))
       (is (lqn:stdstr (let ((gg (stm:new rulex 0)))
                         (stm:itr/all gg #'r/princ)
                         (stm:itr/all gg)))
           "RULEX0RULEX1RULEX2RULEX3hiRULEX 0|>hiRULEX 1|>hiRULEX 2|>hiRULEX 3|>")))

  (stm:with-rules
    ((gx i (values i (if (< i 20) (stm:new gx (1+ i)) t))))
      (let* ((gg (stm:new gx 0)))
        (mvb (g v) (stm:itr/until gg (lambda (i) (= i 10)))  (is (list (functionp g) v) '(t 10)))
        (mvb (g v) (stm:itr/until gg (lambda (i) (= i 100))) (is (list (functionp g) v) '(nil 20)))

        (mvb (g v) (stm:itr/n gg 10)  (is (list (functionp g) v) '(t 9)))
        (mvb (g v) (stm:itr/n gg 100) (is (list (functionp g) v) '(nil 20)))
        (mvb (g v) (stm:itr/all gg)   (is (list (functionp g) v) '(nil 20)))

        (mvb (g v) (stm:itr/n (stm:itr/n gg 10) 3)
             (is (list (functionp g) v) '(t 12)))

        (mvb (g v) (stm:itr/n (stm:itr/until gg (lambda (i) (= i 10))) 2)
             (is (list (functionp g) v) '(t 11)))))

   (stm:with-rules
    ((rx i (values i (stm:new rx (progn (princ :/exec->) (1+ i))))))
    (is (lqn:stdstr (let ((gg (stm:new rx 0)))
                      (stm:itr/n gg 3 #'r/princ)))
        "RX0/EXEC->RX1/EXEC->RX2")))

(subtest "test state machine 2"
  (stm:with-rules
    ((gen-a i (cond ((< i 4)  (values i (stm:new gen-a (1+ i))))
                    ((< i 14) (values i (stm:new gen-a (+ 3 i))))))
     (gen-b l (cond ((and l (cdr l)) (values (car l) (stm:new gen-b (cdr l))))
                    ((car l)         (values (car l) t)))))
    (is (lqn:stdstr
      (let* ((gint (stm:new gen-a 0))
             (gintb1 (stm:itr/n gint   3 (lambda (s r) (lqn:out "~a hi ~a" r s))))
             (gintb2 (stm:itr/n gint   5 (lambda (s r) (lqn:out "hi ~a again ~a." r s))))
             (gintc  (stm:itr/n gintb1 2 (lambda (s r) (lqn:out "~ahello ~a." r s)))))
        (declare (ignorable gintb2))
        (print :--)
        (stm:itr/n gintc 100 (lambda (s r) (lqn:out "~a oh no ~a." r s)))))
      "GEN-A hi 0GEN-A hi 1GEN-A hi 2hi GEN-A again 0.hi GEN-A again 1.hi GEN-A again 2.hi GEN-A again 3.hi GEN-A again 4.GEN-Ahello 3.GEN-Ahello 4.
:-- GEN-A oh no 7.GEN-A oh no 10.GEN-A oh no 13."))

  (stm:with-rules
    ((gx i (values i (if (< i 4) (stm:new gx (1+ i)) t))))
    (let* ((gg (stm:new gx 0)))
     (is (mvl (stm:acc/all gg)) '(nil (4 3 2 1 0)) )
     (is (mvb (g* val) (stm:acc/n gg 2) (list val (functionp g*))) '((1 0) t))

     (is (mvl (stm:acc/all gg #'cons (lambda (s r) (declare (ignore r))
                                       (lqn:fmt "~a/" s))))
         '(nil ("4/" "3/" "2/" "1/" "0/")))
     (is (mvb (g* val) (stm:acc/until gg (lambda (o) (> o 3)))
              (list val (functionp g*)))
         '((4 3 2 1 0) T)))))

(defun which? (i)
  (list i (cond ((and #1=(zerop (mod i 3))
                      #2=(zerop (mod i 5))) :fizzbuzz)
                (#1# :fizz)
                (#2# :buzz)
                (t i))))

(defun fizz-print (s &rest rest)
  (declare (ignore rest))
  (typecase #4=(second s)
            (number (lqn:out " ~a" #4#))
            (t (lqn:out "~%~a" #4#))))

(subtest "test fizzbuzz state machine"
  (stm:with-rules ((fizzbuzz l
                     (values l (stm:new fizzbuzz
                                 (which? (1+ (car l)))))))

    (let ((gg (stm:new fizzbuzz (which? 1))))
      (is (lqn:stdstr
            (stm:itr/n gg 20 #'fizz-print))
" 1 2
FIZZ 4
BUZZ
FIZZ 7 8
FIZZ
BUZZ 11
FIZZ 13 14
FIZZBUZZ 16 17
FIZZ 19
BUZZ"))))

(unless (finalize) (error "error in test state machine"))

