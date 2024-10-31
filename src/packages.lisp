
(defpackage #:stm
  (:use #:common-lisp)
  (:nicknames #:cl-state-machine)
  (:export #:*act*
    #:r/print #:r/print* #:r/identity
    #:? #:new #:with-rules #:later
    #:acc/all #:acc/n #:acc/until
    #:itr/all #:itr/n #:itr/until)
  (:documentation "rule-based (in)finite state machine utilities."))

