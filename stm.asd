(asdf:defsystem #:stm
  :description "Finite State Machine Utilities"
  :version "1.0.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:stm/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:lqn)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "config" :depends-on ("utils"))
               (:file "docs" :depends-on ("config"))
               (:file "stm" :depends-on ("docs"))))

(asdf:defsystem #:stm/tests
  :depends-on (#:uiop #:asdf #:lqn #:stm #:prove)
  :version "1.0.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':stm-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))
