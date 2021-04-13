(defsystem "stepster"
  :version "0.0.7"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("dexador"
               "cl-ppcre"
               "plump"
               "clss")
  :components ((:module "src"
                :components
                ((:file "stepster"))))
  :description "Web tool library"
  :in-order-to ((test-op (test-op "stepster/tests"))))
