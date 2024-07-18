(in-package :cl-user)
(defpackage stepster.urlworks
  (:use :cl)
  (:import-from	:cl-ppcre
   				:scan-to-strings
                :all-matches-as-strings
   				:split   
                :regex-replace)
  (:import-from :stepster.utils
   				:substp
                :string-starts-with
                :last-char)
  (:export
   :same-domain
   :get-arguments
   :arguments-keys
   :arguments-values
   :join-with-main
   :prepare-url
   :get-last
   :query-string
   :make-query-string
   :replace-argument))

(in-package :stepster.urlworks)

(defun same-domain (url domain)  
  (or (substp domain url)
      (relative url)))

(defun prepare-url (url &optional main)
  (cond
    ((substp "http" url) url)
    ((and main (relative url))
     (join-with-main main url))
    (t (http-join url))))

(defun http-join (url &key http-only)
  (concatenate 'string "https://" url))

(defun relative (url)
  (substp "^[/]?[a-zA-Z0-9-/]*[.]?[a-zA-Z0-9-]*$" url))

(defun split-url (url)
  (let ((items (split "[.]|[/]|[?]" url)))
    (loop for item in items
          collect item)))

(defun get-last (url)
  (nth-value 1 (scan-to-strings "(.+\/)(.+)$" url)))

(defun get-main (url)
  (nth-value 1 (scan-to-strings "(.+[.][a-zA-Z0-9]+?)([/]|$)(.*)" url)))

(defun join-with-main (main path)
  (let ((m (aref (get-main main) 0)))
    (concatenate 'string m
                 (unless (or (equal (last-char m) "/")
                             (string-starts-with path "/"))
                   "/")
                 path)))

(defun get-arguments (url)
  (quri:uri-query-params (quri:uri url)))

(defun arguments-keys (list)
  (mapcar #'car (get-arguments list)))

(defun arguments-values (list)
  (mapcar #'cadr (get-arguments list)))

(defun make-query-string (url data)
  (concatenate 'string url "?" (query-string data)))

(defun query-string (data)
  (format nil "~{~{~a=~a~}~^&~}" data))

(defun replace-arguments-with-value (url value)
  (query-string 
   (loop for arg in (get-arguments url)
         collect (list (car arg) value))))

(defun replace-argument (url arg value)
  (let* ((args (get-arguments url))
         (arg-value
           (cadr (assoc (string arg) args :test #'string-equal))))
    (regex-replace arg-value url (string value))))
