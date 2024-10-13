
(in-package #:evl-tests)

(plan 3)

(subtest "test state machine"
   (stm:with-rules ((gx i (cond ((< i 4)  (values i (stm:new gx (1+ i)))))))
     (let ((stm:*act* (lambda (s) (lqn:out "hi~a |>" s))))
       (is (lqn:stdstr (let ((gg (stm:new gx 0)))
                         (stm:itr/all gg #'princ)
                         (stm:itr/all gg)))
           "0123hi0 |>hi1 |>hi2 |>hi3 |>")))

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
    ((gx i (cond ((< i 10) (values i (stm:new gx (progn (princ :/exec)
                                                    (1+ i))))))))
    (is (lqn:stdstr (let ((gg (stm:new gx 0)))
                      (stm:itr/n gg 3 #'princ)))
        "0/EXEC1/EXEC2")))

(subtest "test state machine 2"
  (stm:with-rules
    ((gen-a i (cond ((< i 4)  (values i (stm:new gen-a (1+ i))))
                    ((< i 14) (values i (stm:new gen-a (+ 3 i))))))
     (gen-b l (cond ((and l (cdr l)) (values (car l) (stm:new gen-b (cdr l))))
                    ((car l)         (values (car l) t)))))
    (is (lqn:stdstr
      (let* ((gint (stm:new gen-a 0))
             (gintb1 (stm:itr/n gint   3 (lambda (s) (lqn:out "hi ~a" s))))
             (gintb2 (stm:itr/n gint   5 (lambda (s) (lqn:out "hi again ~a." s))))
             (gintc  (stm:itr/n gintb1 2 (lambda (s) (lqn:out "hello ~a." s)))))
        (declare (ignorable gintb2))
        (print :--)
        (stm:itr/n gintc 100 (lambda (s) (lqn:out "oh no ~a." s)))))
      "hi 0hi 1hi 2hi again 0.hi again 1.hi again 2.hi again 3.hi again 4.hello 3.hello 4.
:-- oh no 7.oh no 10.oh no 13."))

  (stm:with-rules
    ((gx i (values i (if (< i 4) (stm:new gx (1+ i)) t))))
    (let* ((gg (stm:new gx 0)))
      ; (stm:itr/all gg #'princ)

     (is (mvl (stm:acc/all gg)) '(nil (4 3 2 1 0)) )
     (is (mvb (g* val) (stm:acc/n gg 2) (list val (functionp g*))) '((1 0) t))

     (is (mvl (stm:acc/all gg #'cons (lambda (s) (lqn:fmt "~a/" s))))
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

(defun fizz-print (s)
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

