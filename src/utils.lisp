(in-package :cl-user)
(defpackage stepster.utils
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export
   :substp
   :string-starts-with
   :print-error
   :equal-getf
   :reverse-group
   :carlast
   :internks
   :getj
   :flatten
   :member-list
   :clist
   :last-char
   :mklist))

(in-package :stepster.utils)

(defun clist (x &rest args)
  (if args
      (append (list x) args)
      x))

(defun mklist (el)
  (if (consp el)
      el
      (list el)))

(defun last-char (x)
  (make-string 1 :initial-element (aref x (1- (length x)))))

(defun print-error (e)
  (format t "Error:~a~%" e))

(defun setf-assoc (field key value)
  (setf (cdr (assoc key field)) value))

(defun substp (regex string)
  (scan-to-strings regex string))

(defun string-starts-with (string x)
  (string-equal string x :end1 (length x)))

(defun equal-getf (plist indicator)
  (loop for key in plist by #'cddr
        for value in (cdr plist) by #'cddr
        when (equal key indicator)
          return value))

(defun member-list (x y)
  (cond ((null x) t)
        ((atom x)
         (equal x (if (atom y) y (car y))))
        ((atom y)
         (equal x y))
        (t (and (member-list (car x) (car y))
                (member-list (cdr x) (cdr y))))))

(defun reverse-group (source n)
  "Split list into groups by n and reverse them."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (reverse (subseq source 0 n)) acc))
                   (nreverse (cons (reverse source) acc))))))
    (if source (rec source nil) nil)))

(defun carlast (x)
  (car (last x)))

(defun internk (item)
  (cond ((numberp item)
         item)
        ((stringp item)
         (intern item "KEYWORD"))
        (t (intern (string-downcase (string item)) "KEYWORD"))))

(defun internks (item)
  (if (consp item)
      (mapcar #'internk item)
      (internk item)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

; fileworks

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))
