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

(defun findj (list k &optional (acc nil))
    (let ((key (intern-k k)))
        (cond ((null list) nil)
              ((equal (car list) key)
               (cons (reverse (cons (car list) acc))
                     (cadr list)))
              ((consp (cadr list))
               (or (findj (cadr list) k (cons (car list) acc))
                   (findj (cddr list) k acc)))
              ((consp (car list))
               (or (findj (car list) k acc)
                   (findj (cddr list) k acc)))
              (t (findj (cdr list) k acc)))))
