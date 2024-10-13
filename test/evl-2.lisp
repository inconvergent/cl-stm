
(in-package #:evl-tests)

(plan 1)

(subtest "test evl-2"

  (is (evl '(cond ((< 2 1) 7) ((< 1 2) 8) (t :aa))) 8)
  (is (evl '(cond ((< 1 2) 7) ((< 2 1) 8) (t :aa))) 7)
  (is (evl '(cond ((< 2 1) 7) ((< 3 1) 8) (t :aa))) :aa)

  (is (evl '(cond ((< 1 3)))) t)
  (is (evl '(cond ((member :a '(:b :a :c))))) '(:a :c))
  (is (evl '(cond ((member :x '(:b :a :c))))) nil)

  (is (evl '(let (a (b 20)) (list a b))) (list nil 20))
  (is (evl '(let ((a 1) (b 20)) (+ a b))) (+ 1 20))
  (is (evl '(let ((a 1) (b 20)) (+ a b) (- a b))) (- 1 20))

  (is-error (evl '(let ((a 1) (b (1+ a)))
                    (list a b)))
            'warning)

  (is (evl '(let ((fx (lambda (x) (+ 1 x))))
              (fx (fx 1))))
      3)

  (is (evl '(labels ((fact (x) (if (zerop x)
                                   1
                                   (* x (fact (1- x))))))
             (fact 7)))
      5040)
  (is (evl '(labels ((add0 (x) (add1 (sub1 x)))
                     (add1 (x) (1+ x))
                     (sub1 (x) (1- x)))
              (list 333)
              (add0 7)))
      7)

  (is (evl '(destructuring-bind ((xxx yyy) &rest zzz)
                  '((:a :b) :c :d)
              (list :ok yyy xxx zzz)))
      '(:ok :b :a (:c :d)))
  (is (evl '(destructuring-bind ((xxx yyy) &rest zzz)
                  (list (list :a :b) :c :d (1+ 1))
              (list xxx)
              (list :ok xxx yyy zzz)))
      '(:ok :a :b (:c :d 2)))

  (is (evl '(dsb (aa &optional b&b)
                  (list 2)
              (list :ok aa b&b 3)))
      '(:ok 2 nil 3))

  (is (evl '(labels ((fib (n s)
                      (if (>= (length s) n) s
                          (fib n (cons (apply + (subseq s 0 2))
                                       s)))))
              (reverse (fib 10 (list 1 0)))))
      '(0 1 1 2 3 5 8 13 21 34))
  (is (evl '(let ((kk (labels ((yx (b) (1+ b))
                               (fx (a) (yx a)))
                        fx)))
              (kk 33)))
      34))

(unless (finalize) (error "error in test evl-2"))


