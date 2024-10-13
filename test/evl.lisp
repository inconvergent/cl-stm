(in-package #:evl-tests)

(plan 2)

(subtest "evl default env"
  (is-error (evl 's) 'warning)
  (is (evl '1 (lambda (k) (declare (ignore k)) (warn "nop"))) 1)
  (is-error (evl '+ (lambda (k) (declare (ignore k)) (warn "nop"))) 'warning)
  (is-error (evl '(+ 1 2) (lambda (k) (declare (ignore k)) (warn "nop"))) 'warning)
  (is (evl '+) '+)
  (is (evl '(+ 1 2)) 3)
  (is (evl '(let ((a 1) (b 2)) (list b a))) '(2 1))
  (is (with-env () (let ((a 1) (b 2)) (list b a))) '(2 1)))


(subtest "evl custom env"
  (let* ((s 104) (k 107)
         (kv `((s . ,s) (k . ,k)
               (myfx . ,(lambda (k) (+ (sin k) (cos (- k)))))
               (10+ . ,(lambda (x) (+ 10 x)))
               ,@+std-env+)))

    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x))))

      (is-error (evl 'missing #'env) 'error)
      (is-error (evl '(progn missing) #'env) 'error)
      (is (evl '1 #'env) 1)
      (is (evl '+ #'env) '+)
      (is (evl 's #'env) 104)
      (is (evl '(progn s) #'env) 104)
      (is (evl '(list s m) (env/extend-pair '(s m) '(105 106) #'env)) '(105 106))
      (is (evl '((lambda (x) s) -1) #'env) s)
      (is (evl '((lambda (x) s) -1) (env/extend-pair '(s) (list 999) #'env)) 999)

      (is (evl '(+ s k) #'env) (+ s k))
      (is (evl '(myfx 1) #'env) 1.3817732)

      (is (evl '(quote 1) #'env) 1)
      (is (evl '(quote s) #'env) 's)
      (is (evl '(or )) nil)
      (is (evl '(or nil)) nil)
      (is (evl '(or nil :a :b)) :a)
      (is (evl '(and nil :a :b)) nil)
      (is (evl '(and )) t)
      (is (evl '(and t t :b :c)) :c)

      (is-error (evl '(quote) #'env) 'warning)
      (is-error (evl '(quote 1 1) #'env) 'warning)

      (is (evl '(quote (s)) #'env) '(s))
      (is (evl '(quote (+ 1 2)) #'env) '(+ 1 2))
      (is (evl '(quote (+ some-symbol 2)) #'env) '(+ some-symbol 2))
      (is (evl '((lambda (x) (quote s)) 1) #'env) 's)

      (is (evl '(list s k) #'env) `(,s ,k))
      (is (evl '(progn s k :progn) #'env) :progn)

      (is (evl '(if (< 4 1) 2 3) #'env) 3)
      (is (evl '(if (< 4 1) 2) #'env) nil)

      (is (evl '((lambda () 77)) #'env) 77)
      (is (evl '((lambda (x) (+ s x -10000)) 888) #'env) (+ s 888 -10000))
      (is (evl '((lambda (x y) (+ s x y)) 888 999) #'env) (+ s 888 999))
      (is (evl '((lambda (&rest rest) rest) 999 333) #'env) '(999 333))
      (is (evl '(progn s) #'env) s)

      (is (funcall (evl '(lambda (x) x) #'env) 99) 99)
      (is (funcall (evl '(lambda (x) (1- x) x) #'env) 99) 99)
      (is (funcall (evl '(lambda (x) x (1- x)) #'env) 99) 98)

      (is (evl '((lambda (a &optional x)    (list a x)) 999) #'env) '(999 nil))
      (is (evl '((lambda (a &optional x)    (list a x)) 999 777) #'env) '(999 777))
      (is (evl '((lambda (a &optional x) 33 (list a x)) 999 777) #'env) '(999 777))
      (is (evl '(funcall (lambda (a &optional x) (list a x)) 999 777) #'env) '(999 777))
      )))

(unless (finalize) (error "error in test-evl"))
