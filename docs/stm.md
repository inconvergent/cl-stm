## `stm:*act*`
```
 ; STM:*ACT*
 ;   [symbol]
 ; 
 ; *ACT* names a special variable:
 ;   Declared type: FUNCTION
 ;   Value: #<FUNCTION IDENTITY>
 ;   Documentation:
 ;     override function used to process values. eg. #'print
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;   Source file: /data/x/evl/src/stm.lisp
```

## `stm:new`
```
 ; STM:NEW
 ;   [symbol]
 ; 
 ; NEW names a macro:
 ;   Lambda-list: (RULE-NAME VAL-EXPR)
 ;   Documentation:
 ;     new state with this rule and expression. see with-rules.
 ;   Source file: /data/x/evl/src/stm.lisp
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
 ;     all iterators and accumulators accept an act function of one argument which is
 ;       called on each value before it is returned. default: #'identity.
 ;       ; note: to override for the entire context set: evl:*act*.
 ;     
 ;     all accumulators accept an acc and a res option.
 ;       - acc is a function that accepts a value and an accumulated value, then returns
 ;         the new accumualted value. default: #'cons.
 ;     
 ;         ; note: you can write your own function to filter out values. eg:
 ;         ; (lambda (v res)
 ;         ;    (if (my-testp v) (cons v res) res))
 ;     
 ;       - res is the initial value of the accumulation. default: (list).
 ;         res does not have to be a list, but if you override you have to override
 ;         acc to be compatible and vice-versa.
 ; 
 ;   Source file: /data/x/evl/src/stm.lisp
```

