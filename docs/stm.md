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
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     alias: NEW
 ; 
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:acc/all`
```
 ; STM:ACC/ALL
 ;   [symbol]
 ; 
 ; ACC/ALL names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL ACT (ACC (FUNCTION CONS)) RES)
 ;   Declared type: (FUNCTION (FUNCTION &OPTIONAL T FUNCTION T)
 ;                   (VALUES &OPTIONAL (OR NULL FUNCTION) T
 ;                           (OR STM:CND/ALL NULL) &REST T))
 ;   Derived type: (FUNCTION (FUNCTION &OPTIONAL T FUNCTION T)
 ;                  (VALUES (OR NULL FUNCTION) T (OR STM:CND/ALL NULL)
 ;                          &OPTIONAL))
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
 ;   Lambda-list: (STX &OPTIONAL (N 1) ACT (ACC (FUNCTION CONS)) RES)
 ;   Declared type: (FUNCTION (FUNCTION &OPTIONAL FIXNUM T FUNCTION T)
 ;                   (VALUES &OPTIONAL (OR NULL FUNCTION) T
 ;                           (OR STM:CND/ALL NULL) &REST T))
 ;   Derived type: (FUNCTION (FUNCTION &OPTIONAL FIXNUM T FUNCTION T)
 ;                  (VALUES (OR NULL FUNCTION) T (OR STM:CND/ALL NULL)
 ;                          &OPTIONAL))
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
 ;   Lambda-list: (STX &OPTIONAL (UNTIL (FUNCTION IDENTITY)) ACT
 ;                 (ACC (FUNCTION CONS)) RES)
 ;   Declared type: (FUNCTION (FUNCTION &OPTIONAL FUNCTION T FUNCTION T)
 ;                   (VALUES &OPTIONAL (OR NULL FUNCTION) T
 ;                           (OR STM:CND/ALL NULL) &REST T))
 ;   Derived type: (FUNCTION (FUNCTION &OPTIONAL FUNCTION T FUNCTION T)
 ;                  (VALUES (OR NULL FUNCTION) T (OR STM:CND/ALL NULL)
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     accumulate until. see with-rules.
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

## `stm:cnd/all`
```
:missing:

 ; STM:CND/ALL
 ;   [symbol]
 ; 
 ; CND/ALL names the condition-class #<SB-PCL::CONDITION-CLASS STM:CND/ALL>:
 ;   Documentation:
 ;     all conditions to control STM flow.
 ;   Class precedence-list: CND/ALL, CONDITION, SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: CONDITION
 ;   Direct subclasses: CND/DISCARD-OPERATION, CND/STOP-ITR, CND/HALT-ITR
 ;   Direct slots:
 ;     RULE
 ;       Initargs: :RULE
 ;       Readers: CND/RULE
 ;     FLAG
 ;       Initargs: :FLAG
 ;       Readers: CND/FLAG
 ;     OBJ
 ;       Initargs: :OBJ
 ;       Readers: CND/OBJ
 ;     MSG
 ;       Initargs: :MSG
 ;       Readers: CND/MSG
 ; 
```

## `stm:cnd/bind-warn`
```
 ; STM:CND/BIND-WARN
 ;   [symbol]
 ; 
 ; CND/BIND-WARN names a macro:
 ;   Lambda-list: ((VAL CND) EXPR &BODY BODY)
 ;   Documentation:
 ;     (mv)bind val, cnd and warn if cnd is stm:cnd/all.
 ;   Source file: /data/x/stm/src/pre-stm.lisp
 ; 
```

## `stm:cnd/discard-operation`
```
 ; STM:CND/DISCARD-OPERATION
 ;   [symbol]
 ; 
 ; CND/DISCARD-OPERATION names a compiled function:
 ;   Lambda-list: (RULE OBJ &OPTIONAL FLAG &REST MSG)
 ;   Derived type: (FUNCTION
 ;                  (KEYWORD T &OPTIONAL (OR KEYWORD NULL) &REST T)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     discard itr/acc/mutation. see: with-rules/mutate!
 ;   Source file: /data/x/stm/src/pre-stm.lisp
 ; 
 ; CND/DISCARD-OPERATION names the condition-class #<SB-PCL::CONDITION-CLASS STM:CND/DISCARD-OPERATION>:
 ;   Documentation:
 ;     condition to discard operation.
 ;   Class precedence-list: CND/DISCARD-OPERATION, CND/ALL, CONDITION,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: CND/ALL
 ;   No subclasses.
 ;   No direct slots.
 ; 
```

## `stm:cnd/flag`
```
:missing:

 ; STM:CND/FLAG
 ;   [symbol]
 ; 
 ; CND/FLAG names a generic function:
 ;   Lambda-list: (CONDITION)
 ;   Derived type: (FUNCTION (T) *)
 ;   Method-combination: STANDARD
 ;   Methods:
 ;     (CND/FLAG (CND/ALL))
 ; 
```

## `stm:cnd/halt-itr`
```
 ; STM:CND/HALT-ITR
 ;   [symbol]
 ; 
 ; CND/HALT-ITR names a compiled function:
 ;   Lambda-list: (RULE OBJ &OPTIONAL FLAG &REST MSG)
 ;   Derived type: (FUNCTION
 ;                  (KEYWORD T &OPTIONAL (OR KEYWORD NULL) &REST T)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     halt itr/acc. see: with-rules.
 ;   Source file: /data/x/stm/src/pre-stm.lisp
 ; 
 ; CND/HALT-ITR names the condition-class #<SB-PCL::CONDITION-CLASS STM:CND/HALT-ITR>:
 ;   Documentation:
 ;     condition to halt ITR.
 ;   Class precedence-list: CND/HALT-ITR, CND/ALL, CONDITION,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: CND/ALL
 ;   No subclasses.
 ;   No direct slots.
 ; 
```

## `stm:cnd/msg`
```
:missing:

 ; STM:CND/MSG
 ;   [symbol]
 ; 
 ; CND/MSG names a generic function:
 ;   Lambda-list: (CONDITION)
 ;   Derived type: (FUNCTION (T) *)
 ;   Method-combination: STANDARD
 ;   Methods:
 ;     (CND/MSG (CND/ALL))
 ; 
```

## `stm:cnd/rule`
```
:missing:

 ; STM:CND/RULE
 ;   [symbol]
 ; 
 ; CND/RULE names a generic function:
 ;   Lambda-list: (CONDITION)
 ;   Derived type: (FUNCTION (T) *)
 ;   Method-combination: STANDARD
 ;   Methods:
 ;     (CND/RULE (CND/ALL))
 ; 
```

## `stm:cnd/stop-itr`
```
 ; STM:CND/STOP-ITR
 ;   [symbol]
 ; 
 ; CND/STOP-ITR names a compiled function:
 ;   Lambda-list: (RULE OBJ &OPTIONAL FLAG &REST MSG)
 ;   Derived type: (FUNCTION
 ;                  (KEYWORD T &OPTIONAL (OR KEYWORD NULL) &REST T)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     halt itr/acc and stop. see: with-rules.
 ;   Source file: /data/x/stm/src/pre-stm.lisp
 ; 
 ; CND/STOP-ITR names the condition-class #<SB-PCL::CONDITION-CLASS STM:CND/STOP-ITR>:
 ;   Documentation:
 ;     condition to stop ITR.
 ;   Class precedence-list: CND/STOP-ITR, CND/ALL, CONDITION,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: CND/ALL
 ;   No subclasses.
 ;   No direct slots.
 ; 
```

## `stm:dsb`
```
 ; STM:DSB
 ;   [symbol]
 ; 
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     alias: DESTRUCTURING-BIND
 ; 
 ;   Source file: /data/x/stm/src/utils.lisp
 ; 
```

## `stm:itr/all`
```
 ; STM:ITR/ALL
 ;   [symbol]
 ; 
 ; ITR/ALL names a compiled function:
 ;   Lambda-list: (STX &OPTIONAL ACT RES)
 ;   Declared type: (FUNCTION (FUNCTION &OPTIONAL T T)
 ;                   (VALUES &OPTIONAL (OR NULL FUNCTION) T
 ;                           (OR STM:CND/ALL NULL) &REST T))
 ;   Documentation:
 ;     iterate all. see with-rules.
 ;   Inline proclamation: INLINE (inline expansion available)
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
 ;   Declared type: (FUNCTION (FUNCTION &OPTIONAL FIXNUM T T)
 ;                   (VALUES &OPTIONAL (OR NULL FUNCTION) T
 ;                           (OR STM:CND/ALL NULL) &REST T))
 ;   Documentation:
 ;     iterate at most n times. see with-rules.
 ;   Inline proclamation: INLINE (inline expansion available)
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
 ;   Declared type: (FUNCTION (FUNCTION &OPTIONAL FUNCTION T T)
 ;                   (VALUES &OPTIONAL (OR NULL FUNCTION) T
 ;                           (OR STM:CND/ALL NULL) &REST T))
 ;   Documentation:
 ;     iterate until. see with-rules.
 ;   Inline proclamation: INLINE (inline expansion available)
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
 ;   Source file: /data/x/stm/src/pre-stm.lisp
 ; 
```

## `stm:maybe-cnd`
```
:missing:

 ; STM:MAYBE-CND
 ;   [symbol]
 ; 
 ; MAYBE-CND names a type-specifier:
 ;   Documentation:
 ;     null or cnd/all
 ;   Lambda-list: ()
 ;   Expansion: (OR CND/ALL NULL)
 ; 
```

## `stm:mutate!`
```
 ; STM:MUTATE!
 ;   [symbol]
 ; 
 ; MUTATE! names a macro:
 ;   Lambda-list: ((OPR STX &REST REST) &BODY BODY)
 ;   Documentation:
 ;     perform operation and update stx to reference the next state.
 ;     
 ;     ex:
 ;     
 ;      ; equal to mvb but with cnd warnings for cnd/all conditions
 ;      (cnd/bind-warn (val cnd) ; result of itr/n, and conditions if any
 ;                     (mutate! (itr/n sfx 5))
 ;        (print (list val cnd)))
 ;     
 ;     CONDITIONS
 ;     
 ;       mutate! has special handling of cnd/discard-operation. it will handle the
 ;       conditon by not mutating stx, and returning:
 ;     
 ;         values: nil cnd
 ;     
 ;       where cnd can be any cnd/all subcondition. eg. if the operation was halted.
 ;       see with-rules for more details on conditions.
 ; 
 ;   Source file: /data/x/stm/src/pre-stm.lisp
 ; 
```

## `stm:mvb`
```
 ; STM:MVB
 ;   [symbol]
 ; 
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     alias: MULTIPLE-VALUE-BIND
 ; 
 ;   Source file: /data/x/stm/src/utils.lisp
 ; 
```

## `stm:mvc`
```
 ; STM:MVC
 ;   [symbol]
 ; 
 ; MVC names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     alias: MULTIPLE-VALUE-CALL
 ; 
 ;   Source file: /data/x/stm/src/utils.lisp
 ; 
```

## `stm:mvl`
```
 ; STM:MVL
 ;   [symbol]
 ; 
 ; MVL names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     alias: MULTIPLE-VALUE-LIST
 ; 
 ;   Source file: /data/x/stm/src/utils.lisp
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

## `stm:r/acc/val`
```
 ; STM:R/ACC/VAL
 ;   [symbol]
 ; 
 ; R/ACC/VAL names a compiled function:
 ;   Lambda-list: (VAL RES)
 ;   Derived type: (FUNCTION (T T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     the accumulator used in all iterators.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/stm/src/pre-stm.lisp
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
 ;   Source file: /data/x/stm/src/pre-stm.lisp
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
 ;   Source file: /data/x/stm/src/pre-stm.lisp
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
 ;   Source file: /data/x/stm/src/pre-stm.lisp
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
 ;     
 ;     STATE MACHINE CONTEXT WITH RULES/STATES.
 ;     
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
 ;     ITR / ACC - iterators and accumulators
 ;     
 ;       there are three accumulator / iterator functions:
 ;     
 ;       - acc/all acc/n acc/until
 ;       - itr/all itr/n itr/until
 ;     
 ;       NOTE: the iterators are just thinwraps around the corresponding accumulator
 ;             using the r/acc/val function. which just returns the value.
 ;     
 ;       all iterators and accumulators use the act function to process each value
 ;       before it is returned. the default is:
 ;     
 ;       ; (lambda (v rule) v) ; aka #'r/identity, which just returns the value.
 ;     
 ;       NOTE: also see r/print and r/print*, which are useful for development.
 ;       NOTE: to override for the entire context set: stm:*act*.
 ;     
 ;       all accumulators also have an acc and a res option:
 ;     
 ;       - acc is used for accumulation. the default is
 ;     
 ;         ; (lambda (v res) (cons v res)) ; aka. #'cons
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
 ;     ACC/ITR & CONDITIONS
 ;     
 ;       iterators and accumulators have special handling of two conditions
 ;       when signalled in eg act or acc functions:
 ;     
 ;       - cnd/halt-itr, halts iteration, returns values: val stx cnd;
 ;       - cnd/stop-itr, halts iteration, returns values: val nil cnd.
 ; 
 ;   Source file: /data/x/stm/src/stm.lisp
 ; 
```

