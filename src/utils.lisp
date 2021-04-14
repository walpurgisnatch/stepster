(in-package :cl-user)
(defpackage stepster.utils
  (:use :cl)
  (:import-from :cl-ppcre
   				:scan-to-strings)
  (:export
   :regex-group
   :substp
   :string-starts-with
   :print-error))

(in-package :stepster.utils)

(defun print-error (e)
    (format t "Error:~a~%" e))

(defun setf-assoc (field key value)
    (setf (cdr (assoc key field)) value))

(defun regex-group (group vector)
    (aref vector group))

(defun substp (regex string)
    (if (scan-to-strings regex string)
        t
        nil))

(defun string-starts-with (string x)
    (if (string-equal string x :end1 (length x))
        t
        nil))
