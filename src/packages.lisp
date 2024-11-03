
(defpackage #:stm
  (:use #:common-lisp)
  (:nicknames #:cl-state-machine)
  (:export #:*act*
    #:mvc #:mvb #:mvl #:dsb
    #:cnd/halt-itr #:cnd/stop-itr #:cnd/discard-operation
    #:cnd/msg #:cnd/flag #:cnd/rule
    #:cnd/all #:cnd/bind-warn
    #:maybe-cnd
    #:mutate!
    #:r/print #:r/print* #:r/identity #:r/acc/val
    #:? #:new #:with-rules #:later
    #:acc/all #:acc/n #:acc/until
    #:itr/all #:itr/n #:itr/until)
  (:documentation "rule-based (in)finite state machine utilities."))

