(in-package :cl-user)
(defpackage stepster.json-works
  (:use :cl)
  (:export
   :getj
   :findj
   :jfinder
   :internks
   :intern-list
   :get-values
   :pack-with
   :pack-to-json))

(in-package :stepster.json-works)

(defun carlast (x)
    (car (last x))) 

(defun getj (list key)
    (getf list (internk key)))

(defun internk (item)
    (cond ((numberp item)
           item)
          ((stringp item)
           (intern (format nil "~a" item) "KEYWORD"))
        (t (intern (string-downcase (string item)) "KEYWORD"))))

(defun internks (item)
    (if (consp item)
        (mapcar #'internk item)
        (internk item)))

(defun intern2 (item)
    (if (numberp item)
        item
        (intern (string-downcase (string item)))))
    
(defun flatten (x)
    (labels ((rec (x acc)
                 (cond ((null x) acc)
                       ((atom x) (cons x acc))
                       (t (rec (car x) (rec (cdr x) acc))))))
        (rec x nil)))

(defun intern-list (list)
    (loop for item in list
          for i from 1
          if (consp item)
            collect item
          else if (oddp i)
                 collect (internk item)
          else collect (intern2 item)))

(defun pack-to-json (x y)
    (jonathan:to-json (intern-list (pack-with x y))))

(defun pack-with (x y)
    (loop for i in x
          for j in y
          collect i
          collect j))

(defun member-list (x y)
    (cond ((null x) t)
          ((atom x)
           (equal x (if (atom y) y (car y))))
          ((atom y)
           (equal x y))
          (t (and (member-list (car x) (car y))
                  (member-list (cdr x) (cdr y))))))

(defun get-values (list keys)
    (if (consp (car list))
        (loop for i in list
              collect (mapcar #'(lambda (item) (getf i item)) (internks keys)))
        (mapcar #'(lambda (item) (getf list item)) (internks keys))))

(defun findj (json list keys)
    (let ((result (jfinder json (internks list))))
        (if (consp (car result))
            (loop for item in result
                  collect (get-values item keys))
            (get-values result keys))))

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

        
