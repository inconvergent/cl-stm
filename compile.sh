#!/bin/bash

set -e
touch ./stm.asd
time sbcl --quit \
           --eval '(load "stm.asd")'\
           --eval '(handler-case (time (ql:quickload :stm :verbose t))
                     (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.txt 2>&1
