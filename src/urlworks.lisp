(in-package :cl-user)
(defpackage stepster.urlworks
  (:use :cl)
  (:import-from	:cl-ppcre
   				:scan-to-strings
                :all-matches-as-strings
   :split   
                :regex-replace) 
  (:export
   :same-domain
   :get-arguments
   :arguments
   :arguments-values
   :join-with-main))

(in-package :stepster.urlworks)

(defun setf-assoc (field key value)
    (setf (cdr (assoc key field)) value))

(defun regex-group (group vector)
    (aref vector group))

(defun substp (regex string)
    (if (scan-to-strings regex string)
        t
        nil))

;(defun init-url () )

(defun relative (url)
    (substp "^[/]?[a-zA-Z/]*[.]?[a-zA-Z]*$" url))

(defun split-url (url)
    (let ((items (split "[.]|[/]|[?]" url)))
        (loop for item in items
              collect item)))

(defun get-last (url)
    (nth-value 1 (scan-to-strings "(.+)(\/.+)$" url)))

(defun get-main (url)
    (nth-value 1 (scan-to-strings "(.+[.][a-zA-Z0-9]+?)([/]|$)(.*)" url)))

(defun join-with-main (url action)
    (concatenate 'string (aref (get-main url) 0) action))

(defun get-arguments (url)
    (let ((parts (all-matches-as-strings "([a-zA-Z_%0-9-]*?)=.*?(&|$)" url)))
        (mapcar #'(lambda (part) (split "=" (string-trim "&" part))) parts)))

(defun arguments (list)
    (mapcar #'car (get-arguments list)))

(defun arguments-values (list)
    (mapcar #'cadr (get-arguments list)))

(defun replace-arguments (url value)
    (format nil "~{~{~a=~a~}~^&~}" 
           (loop for arg in (get-arguments url)
                   collect (list (car arg) value))))

(defun replace-argument (url arg value)
    (let* ((args (get-arguments url))
           (arg-value
             (cadr (assoc (string arg) args :test #'string-equal))))
        (regex-replace arg-value url (string value))))

(defun same-domain (url domain)
    (or (substp domain url)
        (relative url)))
