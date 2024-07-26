(defsystem "stepster"
  :version "0.1.6"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("dexador"
               "plump"
               "clss"
               "cl-ppcre"
               "babel"
               "jonathan"
               "cl-reexport")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "urlworks")
                 (:file "json-works")
                 (:file "parser")
                 (:file "extensions")
                 (:file "stepster"))))
  :description "Web scraping library")
