
(defpackage #:stm
  (:use #:common-lisp)
  (:nicknames #:cl-state-machine)
  (:export #:*act*
    #:mvc #:mvb #:mvl #:dsb
    #:? #:$ #:new #:with-rules #:later
    #:r/print #:r/print* #:r/identity #:r/acc/val
    #:acc/all #:acc/n #:acc/until
    #:itr/all #:itr/n #:itr/until
    #:mutate!
    #:cnd #:maybe-cnd
    #:cnd/halt-itr #:cnd/stop-itr #:cnd/discard-operation
    #:cnd/msg #:cnd/flag #:cnd/ctx
    #:cnd/all #:cnd/bind-warn)
  (:documentation "rule-based (in)finite state machine utilities."))

