(in-package :cl-user)
(defpackage stepster.json-works
  (:use :cl)
  (:import-from :stepster.utils
                :reverse-group
                :internks
                :carlast
                :member-list)
  (:export
   :getj
   :jfinder
   :internks
   :get-values
   :pack-with
   :pack-to-json
   :collect-json))

(in-package :stepster.json-works)

(defun intern-list (list)
  (loop for item in list
        for i from 1
        if (consp item)
          collect (intern-list item)
        else if (oddp i)
               collect (internk item)
        else collect item))

(defun pack-with (keys values)
  (loop for item in values
        collect (loop for i in keys
                      for j in item
                      collect i
                      collect j)))

(defun pack-to-json (keys values)
  (jonathan:to-json (intern-list (pack-with keys values))))

(defun get-values (list keys)
  (if (consp (car list))
      (loop for i in list
            collect (mapcar #'(lambda (item) (getf i item)) (internks keys)))
      (mapcar #'(lambda (item) (getf list item)) (internks keys))))

(defun jfinder (list key &optional (acc nil))
  (cond ((null list) nil)
        ((and (equal (carlast key) (car list))
              (member-list (cdr (reverse key)) acc))
         (cadr list))
        ((consp (car list))
         (or (jfinder (car list) key acc)
             (jfinder (cdr list) key acc)))
        ((consp (cadr list))
         (or (jfinder (cadr list) key (cons (car list) acc))
             (jfinder (cddr list) key acc)))
        (t (jfinder (cdr list) key acc))))

(defun fj (json keys &optional acc list)    
  (cond ((null json) acc)
        ((equal (car keys) (car json))
         (fj list (cdr keys) (cons (cadr json) acc) list))
        ((every #'consp json)
         (fj (car json) keys (fj (cdr json) keys acc list) (car json)))
        ((consp (cadr json))
         (fj (cadr json) keys (fj (cddr json) keys acc list) list))
        (t (fj (cddr json) keys acc list))))

(defun collect-json (json keys)
  (reverse-group (fj json (internks keys) nil json) (length keys)))

(defun getj (list key)
  (getf list (internks key)))
