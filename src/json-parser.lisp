(in-package :cl-user)
(defpackage stepster.json-parser
  (:use :cl)
  (:export
   :getj))

(in-package :stepster.json-parser)

(defun getj (list key)
    (getf list (intern (string-downcase (string key)) "KEYWORD")))
