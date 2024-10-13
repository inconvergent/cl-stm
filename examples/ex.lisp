#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :evl) ; must be available locally. eg. in ~/common-lisp/evl
(in-package :evl)

(defmacro prt (a &aux (a* (gensym "A")))
  "print expression (a) and the corresponding output. returns the result"
  `(let ((,a* ,a)) (format t "~&>> ~a~%;; ~a~%" ',a ,a*) ,a*))

(defun main ()
  (let ((kv `((s . 104) (k . 107)                            ; custom var
              (myfx . ,(lambda (k) (+ (sin k) (cos (- k))))) ; custom fx
              ,@+std-env+)))
    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x)))
             (evl. (a) (evl a #'env))) ; always use env

      (prt (evl. '1))
      (prt (evl. 's))
      (prt (evl. '(+ s k)))
      (prt (evl. '(myfx 1)))

      (prt (evl. '(quote (+ 1 2))))
      (prt (evl. '(quote 1)))
      (prt (evl. '(quote s)))

      (prt (evl. '(list s k)))
      (prt (evl. '(progn s k :progn)))

      (prt (evl. '(if (< 4 1) 2 3)))
      (prt (evl. '(if (< 4 1) 2)))

      (prt (funcall (evl. '(lambda (x) x)) 99))

      (prt (evl. '((lambda (x) (+ s x -10000)) 888)))
      (prt (evl. '((lambda (x y) (+ s x y)) 888 999)))
      (prt (evl. '((lambda () 77))))

      (prt (evl. '(let ((a 1) (b 20)) (+ a b))))
      (prt (evl. '(let ((a 1) (b 20)) (+ a b) (- a b))))

      (prt (evl. '(let ((fx (lambda (x) (+ 1 x))))
                     (fx (fx 1)))))

      (prt (evl. '(labels ((fact (x) (if (= x 0)
                                         1
                                         (* x (fact (1- x))))))
                    (fact 7))))

      (prt (evl. '(labels ((add0 (x) (add1 (sub1 x)))
                          (add1 (x) (1+ x))
                          (sub1 (x) (1- x)))
                   (add0 7))))

      (prt (evl. '(cond ((< 2 1) 7)
                        ((< 1 2) 8)
                        (t :aa))))

      (prt (evl. '((lambda (&rest rest) rest) 999 333)))

      (evl. '(labels ((fib (n s) (if (>= (length s) n) s
                                     (fib n (cons (apply + (subseq s 0 2))
                                                  (print s))))))
               (fib 10 (list 1 0))))
      )))

(main)

