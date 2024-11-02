#!/bin/bash

set -e
sbcl --version
echo '#### running SBCL tests:'
touch ./stm.asd
time sbcl --noinform  --quit \
     --eval '(ql:quickload :prove)'\
     --eval '(ql:quickload :fset)'\
     --eval '(handler-case (ql:quickload :stm :verbose nil)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (uiop:quit 2)))'\
     --eval '(handler-case (progn (asdf:test-system :stm))
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (uiop:quit 3)))'

cd .. ; touch ./stm.asd

