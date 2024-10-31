(in-package :stm)

; (declaim (list *docstring-map*))
(defvar *docstring-map* (list))

(defmacro -outstr (body) `(with-output-to-string (*standard-output*) ,body))
(defun -strsrt (l) (sort l #'string-lessp :key #'car))

(defun desc (sym) (declare (symbol sym))
  (lqn:qry (lqn:stdstr (describe sym))
           (splt _ (str! #\Newline) nil nil)
           #((fmt " ; ~a~%" _))
           (apply* strcat _)))
(defun docstrings (sym) (declare (symbol sym))
  (apply #'lqn:str! (mapcar (lambda (o) (lqn:str! o #\Newline))
                            (remove-if-not #'identity
                              (mapcar (lambda (ty) (documentation sym ty))
                                      '(variable function setf))))))
(defun select-docs (sym) (declare (symbol sym))
  (let* ((docs (find-if (lambda (c) (eq sym c)) *docstring-map* :key #'car))
         (idocs (docstrings sym))
         (skip (find :skip docs))
         (desc (unless (find :nodesc docs) (desc sym))))
    (values
      (cond (docs (format nil "~&~a~@[~&~%~a~&~]~&" (cadr docs) desc))
            ((and idocs (> (length idocs) 0))
               (format nil "~&~a~@[~&~%~a~&~]~&" desc nil))
            (t (format nil "~&:missing:~%~@[~&~%~a~&~]~&" desc)))
      skip)))

(defmacro pckgs (pkg)
  (with-gensyms (sym)
    `(-strsrt (loop for ,sym being the external-symbols of (find-package ,pkg)
                    collect (list (lqn:str! ,sym) ,sym)))))

(defmacro ext-symbols? (pkg &optional mode)
  "list all external symbols in pkg. use :verbose to inlcude docstring.
use :pretty to print verbose output to stdout in a readable form."
  (with-gensyms (str sym doc skip)
    (case mode
      (:pretty
        `(loop for (,str ,sym) in (pckgs ,pkg)
               do (multiple-value-bind (,doc ,skip)
                        (select-docs ,sym)
                    (unless ,skip (format t "~&## `~(~a:~a~)`~&```~&~a~&```~%~&~%"
                                          (lqn:str! ,pkg) ,str ,doc)))))
      (:pairs `(loop for (,str ,sym) in (pckgs ,pkg)
                     collect (list ,str (select-docs ,sym))))
      (otherwise `(loop for (,str ,sym) in (pckgs ,pkg) collect ,str)))))

(defun map-docstring (&rest rest) (declare (list rest))
  "register docs info associated with symbol (car rest). internal."
  (setf *docstring-map* (remove-if (lambda (cand) (eq (car cand) (car rest)))
                                   *docstring-map*))
  (push rest *docstring-map*))
