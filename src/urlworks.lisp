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
                :regex-group)
  (:export
   :same-domain
   :get-arguments
   :arguments
   :arguments-values
   :join-with-main
   :prepare-url
   :get-last))

(in-package :stepster.urlworks)


(defun same-domain (url domain)  
  (or (substp domain url)
      (relative url)))

(defun prepare-url (url &optional main)
  (cond
    ((and main (relative url))
     (http-join (join-with-main main url)))
    ((string-starts-with url "//") url)
    (t (http-join url))))

(defun http-join (url &key (https nil))
  (let ((https-url (concatenate 'string "https://" url))
        (http-url (concatenate 'string "http://" url)))
    (cond ((substp "http" url) url)            
          (https (handler-case (progn
                                 (dex:get https-url)
                                 https-url)
                   (error () http-url)))
          (t http-url))))        

(defun relative (url)
  (substp "^[/]?[a-zA-Z/]*[.]?[a-zA-Z]*$" url))

(defun split-url (url)
  (let ((items (split "[.]|[/]|[?]" url)))
    (loop for item in items
          collect item)))

(defun get-last (url)
  (nth-value 1 (scan-to-strings "(.+\/)(.+)$" url)))

(defun get-main (url)
  (nth-value 1 (scan-to-strings "(.+[.][a-zA-Z0-9]+?)([/]|$)(.*)" url)))

(defun join-with-main (main path)
  (concatenate 'string (regex-group 0 (get-main main)) path))

(defun get-arguments (url)
  (quri:uri-query-params (quri:uri url)))

(defun arguments (list)
  (mapcar #'car (get-arguments list)))

(defun arguments-values (list)
  (mapcar #'cadr (get-arguments list)))

(defun make-arguments-string (url data)
  (concatenate 'string url "?" (arguments-string data)))

(defun arguments-string (data)
  (format nil "~{~{~a=~a~}~^&~}" data))

(defun replace-arguments (url value)
  (arguments-string 
   (loop for arg in (get-arguments url)
         collect (list (car arg) value))))

(defun replace-argument (url arg value)
  (let* ((args (get-arguments url))
         (arg-value
           (cadr (assoc (string arg) args :test #'string-equal))))
    (regex-replace arg-value url (string value))))
