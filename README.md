# STM - (In)Finite State Machine Utilities

```lisp
  (in-package :stm)

  (with-rules ((ping (values $ (? pong (list (1+ (cadr $)) :pong))))
               (pong (values $ (? ping (list :ping (1+ (car $)))))))

    (let* ((sm (? ping (list :ping 0)))
           (sm10 (itr/n sm 3 #'r/print*)))

      (print :----)
      (itr/n sm10 11 #'r/print*)))
```

examples in [/ex](/ex).

symbol documentation in [/docs/stm.md](/docs/stm/md).
