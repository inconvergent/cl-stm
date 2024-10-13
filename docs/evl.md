## `evl:+std-env+`
```
 ; EVL:+STD-ENV+
 ;   [symbol]
 ; 
 ; +STD-ENV+ names a special variable:
 ;   Value: ((CAR? . CAR?) (ENV/EMPTY . ENV/EMPTY) (ENV/NEW . ENV/NEW)
 ;           (ENV/EXTEND-ALIST . ENV/EXTEND-ALIST)
 ;           (ENV/EXTEND-PAIR . ENV/EXTEND-PAIR) (DEV/DO-OR . DEV/DO-OR)
 ;           (DEV/DO-AND . DEV/DO-AND) (ST/ITR/1 . ST/ITR/1)
 ;           (ST/ITR/N . ST/ITR/N) (ST/ITR/ALL . ST/ITR/ALL)
 ;           (DEV/EVAL-DSB . DEV/EVAL-DSB) (DEV/EVAL-MVB . DEV/EVAL-MVB)
 ;           (DEV/EVAL-LAMBDA . DEV/EVAL-LAMBDA)
 ;           (DEV/EVAL-COERCE-VALUES . DEV/EVAL-COERCE-VALUES)
 ;           (DEV/DO-LABELS . DEV/DO-LABELS) (DEV/DO-LET . DEV/DO-LET)
 ;           (DEV/DO-COND . DEV/DO-COND) (+ . +) (- . -) (/ . /) (* . *)
 ;           (1+ . 1+) (1- . 1-) (T . T) (= . =) (< . <) (<= . <=) (> . >)
 ;           (>= . >=) (EVENP . EVENP) (ODDP . ODDP) (ABS . ABS)
 ;           (MIN . MIN) (MAX . MAX) (SIGNUM . SIGNUM) (FLOOR . FLOOR)
 ;           (ROUND . ROUND) (TRUNCATE . TRUNCATE) (FLOAT . FLOAT)
 ;           (CEILING . CEILING) (SQRT . SQRT) (EXP . EXP) (EXPT . EXPT)
 ;           (LOG . LOG) (MOD . MOD) (REM . REM) (GCD . GCD) (LCM . LCM)
 ;           (SIN . SIN) (COS . COS) (TAN . TAN) (ASIN . ASIN)
 ;           (ACOS . ACOS) (ATAN . ATAN) (SINH . SINH) (COSH . COSH)
 ;           (TANH . TANH) (PI . 3.1415927) (PII . 6.2831855)
 ;           (PI5 . 1.5707964) (EQUAL . EQUAL) (NOT . NOT) (ZEROP . ZEROP)
 ;           (VALUES . VALUES) (VALUES-LIST . VALUES-LIST)
 ;           (IDENTITY . IDENTITY)
 ;           (MULTIPLE-VALUE-CALL . MULTIPLE-VALUE-CALL)
 ;           (MVC . MULTIPLE-VALUE-CALL) (FUNCALL . FUNCALL)
 ;           (MAPCAR . MAPCAR) (MAPC . MAPC) (APPLY . APPLY)
 ;           (PRINT . PRINT) (PRINC . PRINC) (FORMAT . FORMAT)
 ;           (LENGTH . LENGTH) (SUBSEQ . SUBSEQ) (STRING= . STRING=)
 ;           (REVERSE . REVERSE) (LIST . LIST) (CAR . CAR) (CADR . CADR)
 ;           (CDR . CDR) (CONS . CONS) (CDAR . CDAR) (ASSOC . ASSOC)
 ;           (PAIRLIS . PAIRLIS) (ACONS . ACONS) (FIRST . FIRST)
 ;           (LAST . LAST) (SECOND . SECOND) (THIRD . THIRD) (NTH . NTH)
 ;           (INTERSECTION . INTERSECTION)
 ;           (SET-DIFFERENCE . SET-DIFFERENCE) (FIND . FIND)
 ;           (FIND-IF . FIND-IF) (MEMBER . MEMBER) (UNION . UNION)
 ;           (REMOVE-IF . REMOVE-IF) (MAP . MAP) (MAPCAN . MAPCAN)
 ;           (EVERY . EVERY) (SOME . SOME) (APPEND . APPEND)
 ;           (CONCATENATE . CONCATENATE) (ATOM . ATOM) (NULL . NULL)
 ;           (STRINGP . STRINGP) (SYMBOLP . SYMBOLP) (KEYWORDP . KEYWORDP)
 ;           (LISTP . LISTP) (CONSP . CONSP) (NUMBERP . NUMBERP)
 ;           (FUNCTIONP . FUNCTIONP) (ATOM? . ATOM) (NULL? . NULL)
 ;           (EVEN? . EVENP) (ODD? . ODDP) (STR? . STRINGP)
 ;           (SYMBOL? . SYMBOLP) (KEYWORD? . KEYWORDP) (ZERO? . ZEROP)
 ;           (SOME? . SOME) (EVERY? . EVERY) (LIST? . LISTP)
 ;           (CONS? . CONSP) (NUM? . NUMBERP) (FUNCTION? . FUNCTIONP)
 ;           (MEMBER? . MEMBER))
 ;   Documentation:
 ;     convenient standard environment (CL) functions and constant for evl.
 ;     none of them are required.
```

## `evl:car?`
```
 ; EVL:CAR?
 ;   [symbol]
 ; 
 ; CAR? names a compiled function:
 ;   Lambda-list: (L &REST SS)
 ;   Derived type: (FUNCTION (T &REST T) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     t if consp and car is a symbol in ss
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:cons?`
```
:missing:

 ; EVL:CONS?
 ;   [symbol]
```

## `evl:dev/do-and`
```
 ; EVL:DEV/DO-AND
 ;   [symbol]
 ; 
 ; DEV/DO-AND names a compiled function:
 ;   Lambda-list: (EXPR EVL* ENV*)
 ;   Derived type: (FUNCTION (LIST FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     evaluate (and ...) expression with evl* in env*.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/do-cond`
```
 ; EVL:DEV/DO-COND
 ;   [symbol]
 ; 
 ; DEV/DO-COND names a compiled function:
 ;   Lambda-list: (CND X BODY EVL* ENV*)
 ;   Derived type: (FUNCTION (T T LIST FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     recursively evaluate these conds.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/do-labels`
```
 ; EVL:DEV/DO-LABELS
 ;   [symbol]
 ; 
 ; DEV/DO-LABELS names a compiled function:
 ;   Lambda-list: (PAIRS BODY EVL* ENV*)
 ;   Derived type: (FUNCTION (T LIST FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     evaluate this body in an env with these labels (functions).
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/do-let`
```
 ; EVL:DEV/DO-LET
 ;   [symbol]
 ; 
 ; DEV/DO-LET names a compiled function:
 ;   Lambda-list: (VARS BODY EVL* ENV*)
 ;   Derived type: (FUNCTION (T LIST FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     evaluate body in an env with these named variables.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/do-or`
```
 ; EVL:DEV/DO-OR
 ;   [symbol]
 ; 
 ; DEV/DO-OR names a compiled function:
 ;   Lambda-list: (EXPR EVL* ENV*)
 ;   Derived type: (FUNCTION (LIST FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     evaluate (or ...) expression with evl* in env*.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/eval-coerce-values`
```
 ; EVL:DEV/EVAL-COERCE-VALUES
 ;   [symbol]
 ; 
 ; DEV/EVAL-COERCE-VALUES names a compiled function:
 ;   Lambda-list: (EXPR EVL* ENV*)
 ;   Derived type: (FUNCTION (T FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     evaluate ~; coerce all values.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/eval-dsb`
```
 ; EVL:DEV/EVAL-DSB
 ;   [symbol]
 ; 
 ; DEV/EVAL-DSB names a compiled function:
 ;   Lambda-list: (ARGS IN EXPR EVL* ENV*)
 ;   Derived type: (FUNCTION (LIST T T FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
 ;     (evl* '((lambda (,@args*) expr) ,@lst))
 ;     requires that evl* implements (quote ...) and ((lambda ...) ...).
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/eval-lambda`
```
 ; EVL:DEV/EVAL-LAMBDA
 ;   [symbol]
 ; 
 ; DEV/EVAL-LAMBDA names a compiled function:
 ;   Lambda-list: (ARGS BODY EVL* ENV*)
 ;   Derived type: (FUNCTION (T T FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     use CL eval to build a function with these args and body.
 ;     requires that evl* implements (progn ...)
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dev/eval-mvb`
```
 ; EVL:DEV/EVAL-MVB
 ;   [symbol]
 ; 
 ; DEV/EVAL-MVB names a compiled function:
 ;   Lambda-list: (ARGS IN EXPR EVL* ENV*)
 ;   Derived type: (FUNCTION (LIST T T FUNCTION FUNCTION) *)
 ;   Documentation:
 ;     get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
 ;     (evl* '((lambda (,@args*) expr) ,@lst))
 ;     requires that evl* implements (quote ...) and ((lambda ...) ...).
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:dsb`
```
:missing:

 ; EVL:DSB
 ;   [symbol]
 ; 
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:env/empty`
```
:missing:

 ; EVL:ENV/EMPTY
 ;   [symbol]
 ; 
 ; ENV/EMPTY names a compiled function:
 ;   Lambda-list: ()
 ;   Derived type: (FUNCTION NIL *)
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:env/extend-alist`
```
 ; EVL:ENV/EXTEND-ALIST
 ;   [symbol]
 ; 
 ; ENV/EXTEND-ALIST names a compiled function:
 ;   Lambda-list: (A &OPTIONAL (ENV (ENV/NEW)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL FUNCTION)
 ;                  (VALUES FUNCTION &OPTIONAL))
 ;   Documentation:
 ;     new env function extended with this alist.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:env/extend-pair`
```
 ; EVL:ENV/EXTEND-PAIR
 ;   [symbol]
 ; 
 ; ENV/EXTEND-PAIR names a compiled function:
 ;   Lambda-list: (KK VV &OPTIONAL (ENV (ENV/NEW)))
 ;   Derived type: (FUNCTION (LIST LIST &OPTIONAL FUNCTION) *)
 ;   Documentation:
 ;     new env function extended with these names (kk) and values (vv).
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:env/merge`
```
:missing:

 ; EVL:ENV/MERGE
 ;   [symbol]
 ; 
 ; ENV/MERGE names a compiled function:
 ;   Lambda-list: (A B &AUX (S (GENSYM)))
 ;   Derived type: (FUNCTION (T T) (VALUES FUNCTION &OPTIONAL))
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:env/new`
```
 ; EVL:ENV/NEW
 ;   [symbol]
 ; 
 ; ENV/NEW names a compiled function:
 ;   Lambda-list: (&OPTIONAL (A +STD-ENV+))
 ;   Derived type: (FUNCTION (&OPTIONAL LIST) (VALUES FUNCTION &OPTIONAL))
 ;   Documentation:
 ;     create new environment (function) for EVL with this alist.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:even?`
```
:missing:

 ; EVL:EVEN?
 ;   [symbol]
```

## `evl:every?`
```
:missing:

 ; EVL:EVERY?
 ;   [symbol]
```

## `evl:evl`
```
 ; EVL:EVL
 ;   [symbol]
 ; 
 ; EVL names a compiled function:
 ;   Lambda-list: (EXPR &OPTIONAL (ENV (ENV/NEW)))
 ;   Derived type: (FUNCTION (T &OPTIONAL FUNCTION) *)
 ;   Documentation:
 ;     evaluate an EVL expression in env.
 ;     
 ;     arguments:
 ;      - expr: the expression that should be evaluated.
 ;      - env: a funcion used to lookup a variable in scope. see: (evl:env/new)
 ;     
 ;     supports CL syntax:
 ;       - if, and, or, cond, when, unless, progn
 ;       - lambda (lmb), labels (lbl),
 ;       - let, quote, values, multiple-value-list (mvl),
 ;       - destructuring-bind (dsb), multiple-value-bind (mvb),
 ;     
 ;     non CL syntax:
 ;       - (~ ...) coerce value packs to a single values.
 ;       - (~~ fx ...) coerce all values and apply fx.
 ;     
 ;     deviations from regular CL syntax:
 ;       - there is no function name space; variables and functions in environment are
 ;         indistinguishable.
 ;       - inconsistent (left to right) argument evaluation (TODO: check).
 ;       - &optional, &key and &rest are supported as arguments in lambda, labels,
 ;         destructuring-bind. but default values in optional/key are not supported (yet),
 ;         so all defaults are nil.
 ;       - &aux is not supported.
 ; 
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:evl*`
```
 ; EVL:EVL*
 ;   [symbol]
 ; 
 ; EVL* names a compiled function:
 ;   Lambda-list: (EXPR ENV)
 ;   Derived type: (FUNCTION (T FUNCTION) *)
 ;   Documentation:
 ;     evaluate expr in env without error handling. see evl:evl for full docs.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:evl-error`
```
 ; EVL:EVL-ERROR
 ;   [symbol]
 ; 
 ; EVL-ERROR names a compiled function:
 ;   Lambda-list: (EXPR MSG)
 ;   Derived type: (FUNCTION (T T) NIL)
 ;   Documentation:
 ;     raise evl-error condition.
 ;   Source file: /data/x/evl/src/config.lisp
 ; 
 ; EVL-ERROR names the condition-class #<SB-PCL::CONDITION-CLASS EVL:EVL-ERROR>:
 ;   Documentation:
 ;     EVL evaluation error for this expr w/msg.~&
 ;   Class precedence-list: EVL-ERROR, CONDITION, SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: CONDITION
 ;   No subclasses.
 ;   Direct slots:
 ;     EXPR
 ;       Initargs: :EXPR
 ;       Readers: EXPR
 ;     MSG
 ;       Initargs: :MSG
 ;       Readers: MSG
```

## `evl:function?`
```
:missing:

 ; EVL:FUNCTION?
 ;   [symbol]
```

## `evl:keyword?`
```
:missing:

 ; EVL:KEYWORD?
 ;   [symbol]
```

## `evl:lbl`
```
:missing:

 ; EVL:LBL
 ;   [symbol]
```

## `evl:list?`
```
:missing:

 ; EVL:LIST?
 ;   [symbol]
```

## `evl:lmb`
```
 ; EVL:LMB
 ;   [symbol]
 ; 
 ; LMB names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     alias for lambda
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:lst`
```
 ; EVL:LST
 ;   [symbol]
 ; 
 ; LST names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     get all (values ... ) in body as a list.
 ;     almost like multiple-value-list, except it handles multiple arguments.
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:member?`
```
:missing:

 ; EVL:MEMBER?
 ;   [symbol]
```

## `evl:mvb`
```
:missing:

 ; EVL:MVB
 ;   [symbol]
 ; 
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:mvl`
```
:missing:

 ; EVL:MVL
 ;   [symbol]
 ; 
 ; MVL names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:null?`
```
:missing:

 ; EVL:NULL?
 ;   [symbol]
```

## `evl:num?`
```
:missing:

 ; EVL:NUM?
 ;   [symbol]
```

## `evl:odd?`
```
:missing:

 ; EVL:ODD?
 ;   [symbol]
```

## `evl:some?`
```
:missing:

 ; EVL:SOME?
 ;   [symbol]
```

## `evl:str?`
```
:missing:

 ; EVL:STR?
 ;   [symbol]
```

## `evl:symbol?`
```
:missing:

 ; EVL:SYMBOL?
 ;   [symbol]
```

## `evl:v?`
```
 ; EVL:V?
 ;   [symbol]
 ; 
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T) &AUX
 ;                 (V
 ;                  (SLOT-VALUE (FIND-SYSTEM (QUOTE EVL))
 ;                              (QUOTE VERSION))))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     return/print evl version.
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:with-env`
```
 ; EVL:WITH-ENV
 ;   [symbol]
 ; 
 ; WITH-ENV names a macro:
 ;   Lambda-list: ((&OPTIONAL (ENV (ENV/NEW))) &BODY BODY)
 ;   Documentation:
 ;     evaluate '(progn ,@body) in env with error handling.
 ;   Source file: /data/x/evl/src/interp.lisp
```

## `evl:zero?`
```
:missing:

 ; EVL:ZERO?
 ;   [symbol]
```

## `evl:~`
```
 ; EVL:~
 ;   [symbol]
 ; 
 ; ~ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     wraps rest in (mvc #'values ...).
 ;   Source file: /data/x/evl/src/utils.lisp
```

## `evl:~~`
```
 ; EVL:~~
 ;   [symbol]
 ; 
 ; ~~ names a macro:
 ;   Lambda-list: (FX &REST REST)
 ;   Documentation:
 ;     (mvc fx (~ ,@rest)).
 ;   Source file: /data/x/evl/src/utils.lisp
```

