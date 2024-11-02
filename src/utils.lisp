(in-package :stm)

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args) ,(lqn:fmt "alias: ~s~&" long) `(,',long ,@args)))
(abbrev mvc multiple-value-call) (abbrev mvb multiple-value-bind)
(abbrev mvl multiple-value-list) (abbrev dsb destructuring-bind)

(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'stm) 'asdf:version)))
  "return/print STM version." (unless silent (format t "~&STM version: ~a~%." v)) v)

(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

