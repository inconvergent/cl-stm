(in-package #:evl-tests)

(plan 1)

(let ((kv `((mfx . ,(lambda (x) (* 1000 x)))
            (xx . 12) (yy . 13)
            ,@evl:+std-env+)))

(subtest "evl-evl"
  (labels ((env (x) (let ((res (assoc x kv)))
                      (if res (cdr res)
                              (error "[EVL]: undefined variable: ~a" x)))))
    (is (evl:evl
          `(labels
             ((env. (x) (let ((res (assoc x '((yy . 13000) ,@kv))))
                          (if res (cdr res)
                                  (error "[*EVL*]: undefined variable: ~a" x))))
              (evl. (expr env.)
                ; simplified implementation of EVL in EVL
                (cond ((null expr) expr)
                      ((str? expr) expr)
                      ((num? expr) expr)
                      ((function? expr) expr)
                      ((keyword? expr) expr)
                      ((symbol? expr) (env. expr))

                      ((car? expr 'quote) (cadr expr))

                      ((car? expr 'progn)
                       (destructuring-bind (a &rest rest) (cdr expr)
                         (if rest (progn (evl. a env.) (evl. (cons 'progn rest) env.))
                                  (evl. a env.))))

                      ((car? expr 'dsb*) ; SIC
                       (dsb (vars in &rest rest) (cdr expr)
                         (dev/eval-dsb vars in rest evl. env.)))

                      ((car? expr 'if)
                       (dsb (test then &optional else) (cdr expr)
                         (if (evl. test env.) (evl. then env.) (evl. else env.))))

                      ((car? expr 'cond)
                       (dsb ((cnd x) &rest rest) (cdr expr)
                         (dev/do-cond cnd x rest evl. env.)))

                      ((car? expr 'lambda)
                       (dsb (kk &rest rest) (cdr expr)
                         (dev/eval-lambda kk rest evl. env.)))

                      ((car? expr 'labels*) ; SIC
                       (dsb (pairs &rest body) (cdr expr)
                         (dev/do-labels pairs body evl. env.)))

                      ((car? expr 'let)
                       (dsb (vars &rest body) (cdr expr)
                         (dev/do-let vars body evl. env.)))

                      ((cons? expr)
                       (apply (evl. (car expr) env.)
                              (mapcar (lmb (x) (evl. x env.))
                                      (cdr expr)))))))
             (list (evl. 'xx env.)
                   (evl. 'yy env.)
                   (evl. '(progn xx) env.)
                   (evl. '(progn yy) env.)
                   (evl. '(let ((xx -11) (yy :hello)) (list xx yy)) env.)
                   (evl. '(mfx xx) env.)
                   (evl. '(mfx yy) env.)
                   (evl. '((lambda (x) x) :val) env.)
                   (evl. '(labels* ((add0 (x) (add1 (sub1 x)))
                                    (add1 (x) (1+ x))
                                    (sub1 (x) (1- x)))
                            (add0 7)) env.)
                   (evl. '(cond ((< 1 2) :yes)) env.)
                   (evl. '(cond ((< 2 1) :yes) ((> 2 1) :no)) env.)
                   (evl. '(dsb* (x) '(11) x) env.)
                   (evl. '(dsb* (&rest rest) '(1 2 3) rest) env.)))
          #'env)
        '(12 13000 12 13000 (-11 :hello) 12000 13000000 :val 7
          :yes :no 11 (1 2 3))))))

(unless (finalize) (error "error in test-evl-evl"))



