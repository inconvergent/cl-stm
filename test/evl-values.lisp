(in-package #:evl-tests)

(plan 2)

(subtest "evl-values-utils"
   (is-values (~ (values 9 3 2) (values 5 5)) '(9 3 2 5 5))
   (is (~~ list (values 8 3 1)) '(8 3 1))
   (is (~~ list (values 8 3) (values 7 3)) '(8 3 7 3))
   (is (mvl (values 7 3)) '(7 3))
   (is (lst (values 7 3) (values 8 3)) '(7 3 8 3)))

(subtest "evl-values"

   (is (multiple-value-list
         (evl '(values 1 2 3))) '(1 2 3))

   (is (evl '(multiple-value-list
               (let ((a (list :x :y :z)))
                 (values (car a) (cdr a)))))
       '(:x (:y :z)))

   (is (evl '(multiple-value-list
               (let ((a (list :x :y :z)))
                 (~ (car a) (cdr a)))))
       '(:x (:y :z)))

   (is (mvl (evl '(values 1 (values 2 3) 4))) '(1 2 4))
   (is (mvl (evl '(~ 1 (values 2 3) 4))) '(1 2 3 4))

   (is (multiple-value-list
         (evl '(~ 1 (~ 2 (~ 3 4)) 5))) '(1 2 3 4 5))
   (is (multiple-value-list
         (evl '(~ 1 2 (~ (+ 3 4) (~ 3 4)) 5))) '(1 2 7 3 4 5))

   (is (evl '(multiple-value-bind (a b)
                (values 1 2) (list b a)))
       '(2 1))

   (is (evl '(multiple-value-bind (a b c)
                (values 1 2 (values 3 4)) (list b a c)))
       '(2 1 3))
   (is (multiple-value-list
         (funcall (evl '(lambda () (values 1 2 3)))))
       '(1 2 3)

    (is (multiple-value-list
          (evl '(multiple-value-bind (a b)
                   (values 1 2) (values b a))))
        '(2 1))
    (is (evl '(multiple-value-list (values 1 2))) '(1 2))
    (is (evl '(mvl (mvb (a b) (values 1 2) (values b a))))
        '(2 1))

    (is (evl '(evl:mvl (evl:mvb (a b) (~ 1 2 ) (~ b a)))) '(2 1))
    (is (multiple-value-list (evl '(progn (values 3 2 1)
                                           (values -3 -2 -7))))
        '(-3 -2 -7))
    (is (evl '(mvl (progn (values 3 2 1) (values -3 -2 -6))))
        '(-3 -2 -6))
    (is (mvl (evl '(~~ (lambda (x y z w) (values z y x w))
                         (values 1 2) (values 3 4) )))
        '(3 2 1 4))

    (is (mvl (evl '(~~ values (values 1 2) (values 3 4))))
        '(1 2 3 4))

    (is (evl '(mvl (~~ values (values 1 2) (values 3 4)))) '(1 2 3 4))
    (is (evl '(mvl (~~ values (~ (values 1 2) (values 3 4))))) '(1 2 3 4))

    (is (evl '(mvl (cond ((< 1 2) (values :va :vb))))) '(:va :vb))
    (is (evl '(mvl (cond ((< 1 2) (values :va :vb))))) '(:va :vb))
    (is (evl '(mvl (cond (nil t) ((values :ka :kb))))) '(:ka))
    (is-values (evl '#1=(or (values :mvla :mvlb))) (multiple-value-list #1#))
    (is-values (evl '#2=(and (values :mvla :mvlb))) (multiple-value-list #2#))
    (is-values (evl '(and (~ :mvla :mvlb))) (multiple-value-list (and (values :mvla :mvlb))))
    ))

(unless (finalize) (error "error in evl-values"))
