(defsystem "stepster"
    :version "0.1.0"
    :author "Walpurgisnatch"
    :license "MIT"
    :depends-on ("dexador"
                 "cl-ppcre"
                 "plump"
                 "clss")
    :components ((:module "src"
                  :serial t
                  :components
                          ((:file "stepster")
                           (:file "urlworks")
                           (:file "parser"))))
    :description "Web tool library"
    :in-order-to ((test-op (test-op "stepster/tests"))))
