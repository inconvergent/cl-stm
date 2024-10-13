#!/bin/bash

set -e
touch ./evl.asd
time sbcl --quit \
           --eval '(load "evl.asd")'\
           --eval '(handler-case (time (ql:quickload :evl :verbose t))
                     (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.txt 2>&1
