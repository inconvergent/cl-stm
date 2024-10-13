
(in-package :evl/code)

(defun rndi (a)
  (declare (fixnum a)) "random fixnum in range (0 a]."
  (the fixnum (random a)))

(defun rndget (l)
  (declare (sequence l)) "get random item from sequence l."
  (etypecase l (list (nth (rndi (length (the list l))) l))
               (vector (aref l (rndi (length l))))))

; (veq:fvdef trunc (v &optional (s 10000.0)) (veq:fclamp* v (- s) s))
; (defun rnd (a &aux (a* (abs a)))
;   (if (zerop a*) 0f0 (rnd:rnd* a*)))

(defun signatures (exprs &aux (ht (make-hash-table :test #'equal)))
  (labels ((add-fx (s fx-args)
             (setf (gethash s ht) (push fx-args (gethash s ht (list))))))
    (lqn:qry exprs #((add-fx (car _) (second _))))
    ht))

(defun !signp (c) (and (symbolp c) (lqn:pref? (lqn:str! c) "!"))) ; t if ! prefix

(defun @resym (s) (lqn:sym! "@" (lqn:seq (lqn:str! s) 1))) ; strip first char

(defun gen (signs s)
  "generate new expressions using these "
  (labels ((do-eval (c)
              (handler-case (eval `(veq:fvprogn ,(rndget (gethash (@resym c) signs))))
                (error (e) (error "DO-EVAL ERROR for ~a:~%  err:~%  ~a" c e))))
           (eval-values (c &aux (v (veq:lst (do-eval c))))
             (cond ((and (listp v) (> (length v) 1)) `(values ,@v))
                   ((listp v) (car v))
                   (t v))) ; unreachable
           (rec (c &optional (d 0))
             (cond ((> d 10) c)
                   ((null c) c)
                   ((!signp c) (rec (rndget (gethash c signs)) (1+ d)))
                   ((symbolp c) c) ((numberp c) c) ((vectorp c) c)
                   ((listp c) (cons (rec (car c) (1+ d))
                                    (rec (cdr c) (1+ d))))))
           (rec/fill-vals (c)
             (cond ((null c) c)
                   ((!signp c) (eval-values c))
                   ((symbolp c) c) ((numberp c) c) ((vectorp c) c)
                   ((listp c) (cons (rec/fill-vals (car c))
                                    (rec/fill-vals (cdr c)))))))
    (rec/fill-vals (rec (rndget (gethash s signs))))))

