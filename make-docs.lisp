#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :stm :silent t)
(in-package #:stm)

(defun import-all (fn)
  (with-open-file (f (mkstr fn ".lisp") :direction :input)
    (loop for o = (read f nil) while o collect o)))
(defun internal-path (path) (namestring (asdf:system-relative-pathname :stm path)))

(defun make-docs ()
  (loop for (o . rest) in (import-all (internal-path "src/packages"))
        for pkg = (mkstr (car rest))
        for fn = (internal-path (format nil "docs/~(~a~).md" (lqn::repl pkg "/" "-")))
        if (eq o 'defpackage)
        do (format t "~&~a~%" fn)
           (with-open-file (f fn :direction :output :if-exists :supersede)
             (princ (-outstr (ext-symbols? pkg :pretty)) f))))
(make-docs)

