(in-package :cl-user)
(defpackage stepster.json-works
  (:use :cl)
  (:export
   :getj
   :findj
   :jfinder
   :internk
   :intern-list
   :pack-with))

(in-package :stepster.json-works)

(defun carlast (x)
    (car (last x))) 

(defun getj (list key)
    (getf list (internk key)))

(defun internk (item)
    (intern (string-downcase (string item)) "KEYWORD"))

(defun intern2 (item)
    (intern (string-downcase (string item))))

(defun internks (item)
    (mapcar #'internk item))

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
            collect (mapcar #'intern2 item)
          else if (oddp i)
                 collect (internk item)
          else collect (intern2 item)))

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

(defun findj (list keys)
    (flet ((collect2 (list keys)
               (mapcar #'(lambda (item) (getf list item)) keys)))
        (flatten (loop for (x y) in keys
              with result do
                (setf result (jfinder list (internks x)))
              if (consp (car result))
                collect (loop for item in result
                              collect (collect2 item (internks y)))
              else collect (collect2 result (internks y))))))
        

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

        
