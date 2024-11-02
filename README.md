# STM - (In)Finite State Machine Utilities

```lisp
  (in-package :stm)

  (with-rules ((ping l (values l (? pong (list (1+ (cadr l)) :pong))))
               (pong l (values l (? ping (list :ping (1+ (car l)))))))

    (let* ((sm (? ping (list :ping 0)))
           (sm10 (itr/n sm 3 #'r/print*)))

      (print :----)
      (itr/n sm10 11 #'r/print*)))
```

examples in [/ex](/ex).
