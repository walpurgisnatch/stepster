(in-package :cl-user)
(defpackage stepster.json-works
  (:use :cl)
  (:export
   :getj
   :findj
   :jfinder
   :intern-k))

(in-package :stepster.json-works)

(defun carlast (x)
    (car (last x))) 

(defun getj (list key)
    (getf list (intern-k key)))

(defun intern-k (item)
    (intern (string-downcase (string item)) "KEYWORD"))

(defun intern-ks (item)
    (mapcar #'intern-k item))

(defun flatten (x)
    (labels ((rec (x acc)
                 (cond ((null x) acc)
                       ((atom x) (cons x acc))
                       (t (rec (car x) (rec (cdr x) acc))))))
        (rec x nil)))

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
                (setf result (jfinder list (intern-ks x)))
              if (consp (car result))
                collect (loop for item in result
                              collect (collect2 item (intern-ks y)))
              else collect (collect2 result (intern-ks y))))))
        

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
