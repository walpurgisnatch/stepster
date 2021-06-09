(in-package :cl-user)
(defpackage stepster.json-works
  (:use :cl)
  (:export
   :getj
   :findj
   :intern-k))

(in-package :stepster.json-works)

(defun getj (list key)
    (getf list (intern-k key)))

(defun intern-k (item)
    (intern (string-downcase (string item)) "KEYWORD"))

(defun member-list (x y)
    (cond ((null x) t)
          ((atom x)
           (equal x (if (atom y) y (car y))))
          ((atom y)
           (equal x y))
          (t (and (member-list (car x) (car y))
                  (member-list (cdr x) (cdr y))))))


(defun findj (list ks k &optional (path nil) (acc nil))
    (let* ((key (intern-k k))
           (chain (mapcar #'intern-k path))
           (keys (mapcar #'intern-k ks))
           (result (jfinder list key chain acc)))
        (print result)
        (loop for item in result
              collect (mapcar #'(lambda (i) (getf item i)) keys))))
        

(defun jfinder (list key &optional (chain nil) (acc nil))
    (cond ((null list) nil)
          ((and (equal (car list) key)
                (or (member-list (reverse chain) acc)
                    (null chain)))
           (cadr list))
          ((consp (car list))
           (or (jfinder (car list) key chain acc)
               (jfinder (cdr list) key chain acc)))
          ((consp (cadr list))
           (or (jfinder (cadr list) key chain (cons (car list) acc))
               (jfinder (cddr list) key chain acc)))
          (t (jfinder (cdr list) key chain acc))))
