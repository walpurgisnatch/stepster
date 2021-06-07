(in-package :cl-user)
(defpackage stepster.json-works
  (:use :cl)
  (:export
   :getj))

(in-package :stepster.json-works)

(defun getj (list key)
    (getf list (intern (string-downcase (string key)) "KEYWORD")))
