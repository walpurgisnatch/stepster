(in-package :cl-user)
(defpackage stepster.extensions 
  (:use :cl :stepster.parser)
  (:import-from :stepster.utils
                :pathname-as-directory)
  (:export
   :download-all-images
   :extract-urls
   :extract-input-names
   :extract-js-src
   :parse-regex
   :parse-text))

(in-package :stepster.extensions)

(defmacro for-js (page &body body)
  `(let ((srcs (extract-js-src (parse ,page))))
     (loop for src in srcs do
       (let ((js-file (babel:octets-to-string (safe-get src) :encoding :utf-8)))
         (progn ,@body)))))

(defun parse-text (page selectors)
  (let* ((parsed (parse page))
         (finded (clss:select (nodes-to-string selectors) parsed)))
    (when (> (length finded) 0) (plump:text (elt finded 0)))))

(defun parse-json (page key)
  (jfinder (parse page) key))

(defun extract-urls (page &optional test arg)
  (collect-from (parse page) 'a :attr 'href :test test :test-args arg))

(defun extract-input-names (page)
  (collect-from (parse page) '(form input) :attr 'name))

(defun extract-js-src (page)
  (collect-from (parse page) 'script :attr 'src))

(defun download-all-images (url dir)
  (let ((images (collect-from (parse url) '(img src))))
    (setf dir (namestring (ensure-directories-exist (pathname-as-directory dir))))
    (loop for image in images
          when image
            do (handler-case
                   (let ((filename (concatenate 'string dir (aref (get-last image) 1))))
                     (download-file image filename))
                 (error (e) (format t "~&Error while downloading image [~a]~%~a~%" image e))))))

(defun parse-regex (url regex)
  (cl-ppcre:all-matches-as-strings regex (page-text url)))
