(in-package :cl-user)
(defpackage stepster.utils
  (:use :cl)
  (:import-from :cl-ppcre
   				:scan-to-strings)
  (:export
   :regex-group
   :substp
   :string-starts-with
   :print-error
   :equal-getf))

(in-package :stepster.utils)

(defun print-error (e)
    (format t "Error:~a~%" e))

(defun setf-assoc (field key value)
    (setf (cdr (assoc key field)) value))

(defun regex-group (group vector)
    (aref vector group))

(defun substp (regex string)
    (if (scan-to-strings regex string)
        t
        nil))

(defun string-starts-with (string x)
    (if (string-equal string x :end1 (length x))
        t
        nil))

(defun equal-getf (plist indicator)
  (loop for key in plist by #'cddr
        for value in (cdr plist) by #'cddr
        when (equal key indicator)
        return value))

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
