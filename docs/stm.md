## `stm:*act*`
```
 ; STM:*ACT*
 ;   [symbol]
 ; 
 ; *ACT* names a special variable:
 ;   Declared type: FUNCTION
 ;   Value: #<FUNCTION R/IDENTITY>
 ;   Documentation:
 ;     function that is called for each iteration. requires
 ;     two arguments. the first argument is the value. must return the desired return
 ;     value for each iteration.
 ;     the second is the (keyword) name of the current rule.
 ; 
```

## `stm:?`
```
 ; STM:?
 ;   [symbol]
 ; 
 ; ? names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     alias for new.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:acc/all`
```
 ; STM:ACC/ALL
 ;   [symbol]
 ; 
 ; ACC/ALL names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL (ACC (FUNCTION CONS)) ACT RES)
 ;   Derived type: (FUNCTION (T &OPTIONAL FUNCTION T T)
 ;                  (VALUES NULL T &OPTIONAL))
 ;   Documentation:
 ;     accumulate all. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:acc/n`
```
 ; STM:ACC/N
 ;   [symbol]
 ; 
 ; ACC/N names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL (N 1) (ACC (FUNCTION CONS)) ACT RES)
 ;   Derived type: (FUNCTION (T &OPTIONAL FIXNUM FUNCTION T T)
 ;                  (VALUES T T &OPTIONAL))
 ;   Documentation:
 ;     accumulate at most n times. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:acc/until`
```
 ; STM:ACC/UNTIL
 ;   [symbol]
 ; 
 ; ACC/UNTIL names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL (UNTIL (FUNCTION IDENTITY))
 ;                 (ACC (FUNCTION CONS)) ACT RES)
 ;   Derived type: (FUNCTION (T &OPTIONAL FUNCTION FUNCTION T T)
 ;                  (VALUES (OR NULL FUNCTION) T &OPTIONAL))
 ;   Documentation:
 ;     accumulate until. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:itr/all`
```
 ; STM:ITR/ALL
 ;   [symbol]
 ; 
 ; ITR/ALL names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL ACT RES)
 ;   Derived type: (FUNCTION (T &OPTIONAL T T) *)
 ;   Documentation:
 ;     iterate all. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:itr/n`
```
 ; STM:ITR/N
 ;   [symbol]
 ; 
 ; ITR/N names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL (N 1) ACT RES)
 ;   Derived type: (FUNCTION (T &OPTIONAL FIXNUM T T) *)
 ;   Documentation:
 ;     iterate at most n times. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:itr/until`
```
 ; STM:ITR/UNTIL
 ;   [symbol]
 ; 
 ; ITR/UNTIL names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL (UNTIL (FUNCTION IDENTITY)) ACT RES)
 ;   Derived type: (FUNCTION (T &OPTIONAL FUNCTION T T) *)
 ;   Documentation:
 ;     iterate until. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:later`
```
 ; STM:LATER
 ;   [symbol]
 ; 
 ; LATER names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     wrap expression in (lambda () ...) to evaluate later.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:new`
```
 ; STM:NEW
 ;   [symbol]
 ; 
 ; NEW names a macro:
 ;   Lambda-list: (NAME EXPR)
 ;   Documentation:
 ;     new state with this rule and expression. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:r/identity`
```
 ; STM:R/IDENTITY
 ;   [symbol]
 ; 
 ; R/IDENTITY names a compiled function:
 ;   Lambda-list: (V RULE)
 ;   Derived type: (FUNCTION (T KEYWORD) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     default function for act / *act*.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:r/print`
```
 ; STM:R/PRINT
 ;   [symbol]
 ; 
 ; R/PRINT names a compiled function:
 ;   Lambda-list: (V RULE)
 ;   Derived type: (FUNCTION (T KEYWORD) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     print rule and value. return v.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:r/print*`
```
 ; STM:R/PRINT*
 ;   [symbol]
 ; 
 ; R/PRINT* names a compiled function:
 ;   Lambda-list: (V RULE)
 ;   Derived type: (FUNCTION (T KEYWORD) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     print rule and value. return v.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:with-rules`
```
 ; STM:WITH-RULES
 ;   [symbol]
 ; 
 ; WITH-RULES names a macro:
 ;   Lambda-list: (RULES &BODY BODY)
 ;   Documentation:
 ;     state machine context with rules/states.
 ;     ex:
 ;     
 ;       ; (with-rules
 ;       ;   ((ping l (values l (new pong (list (1+ (cadr l)) :pong))))
 ;       ;    (pong l (values l (new ping (list :ping (1+ (car l)))))))
 ;     
 ;       ;   (let* ((sm0 (new ping `(:ping 0)))  ; initial value. not evaluated here
 ;       ;          (sm3 (itr/n sm0 3 #'princ))) ; eval & print 3 ping-pongs
 ;       ;     (itr/n sm3 11 #'print)))          ; eval & print the next 11
 ;     
 ;     see iterators and accumulators:
 ;       - acc/all acc/n acc/until
 ;       - itr/all itr/n itr/until
 ;     
 ;     all iterators and accumulators use the act function to process each value
 ;       before it is returned. the default is:
 ;     
 ;       ; (lambda (v rule) v) ; aka #'r/identity, which just returns the value.
 ;     
 ;       NOTE: also see r/print and r/print*, which are useful for development.
 ;       NOTE: to override for the entire context set: stm:*act*.
 ;     
 ;     all accumulators also have an acc and a res option:
 ;       - acc is used for accumulation. the default is
 ;     
 ;         ; (lambda (v rule res) (cons v res)) ; aka. #'r/cons
 ;     
 ;         NOTE: you can filter which values are accumulated like this:
 ;     
 ;         ; (lambda (v rule res)
 ;         ;   (if (my-testp rule v) (cons v res) res))
 ;     
 ;       - res is the initial value of the accumulation. default: (list).
 ;         res does not have to be a list, but if you override you have to override
 ;         acc to be compatible and vice-versa.
 ; 
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

