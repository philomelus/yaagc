(in-package :asdf-user)
(defsystem "yaagc-tests"
  :description "Test suite for the yaagc"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :depends-on (:yaagc
               :fiveam)
  :pathname "tests/"
  :license "MIT"
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "yaagc-tests"))))

  :perform (test-op (o c) (symbol-call :yaagc-tests :run-all-tests)))
