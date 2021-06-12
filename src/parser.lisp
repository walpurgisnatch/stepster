(in-package :cl-user)
(defpackage stepster.parser 
  (:use :cl)
  (:import-from	:stepster.urlworks
                :same-domain
                :join-with-main
                :prepare-url
                :make-arguments-string
                :get-last)
  (:import-from :stepster.json-works
                :getj)
  (:import-from :stepster.utils
                :print-error
                :substp
                :equal-getf
                :regex-group
                :pathname-as-directory)
  (:export
   :parse
   :attribute
   :collect-from
   :submit-form
   :download-file
   :download-page
   :download-all-images
   :fill-form
   :extract-forms
   :parse-json
   :get-json
   :post-json
   :safe-get
   :safe-post))

(in-package :stepster.parser)


(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))


(defmacro with-get (url &body body)
    `(handler-case
         (multiple-value-bind (response-body status-code response-headers quri-uri)
             (dex:get (prepare-url ,url) :cookie-jar *cookie-jar*)
             (declare (ignorable status-code response-headers quri-uri))
             (let ((root-node (plump:parse response-body))
                   (response-url (quri:render-uri quri-uri)))
                 (declare (ignorable root-node))
                 (progn ,@body)))
       (error (e) (print-error e))))

(defmacro for-js (page &body body)
    `(let ((srcs (extract-js-src (parse ,page))))
         (loop for src in srcs do
           (let ((js-file (babel:octets-to-string (safe-get src) :encoding :utf-8)))
               (progn ,@body)))))

(defun parse (url)
    "Return plump root node for given url"
    (if (stringp url)
        (plump:parse (safe-get url))
        url))    

(defun safe-get (url)
    (handler-case (dex:get (prepare-url url)
                           :cookie-jar *cookie-jar*)
      (error (e) (print-error e))))

(defun safe-post (url data)
    (handler-case
        (dex:post url
                  :cookie-jar *cookie-jar*
                  :content data)
      (error (e) (print-error e))))

(defun submit-form (url &key form data)
    (let* ((form (extract-forms (parse url) (when form form)))
           (action (join-with-main url (attribute form 'action)))
           (method (attribute form 'method))
           (data (fill-form form data)))
        (if (string-equal method "post")
            (safe-post action data)
            (safe-get (make-arguments-string action data)))))

(defun get-json (url)
    (jonathan:parse (safe-get url)))

(defun post-json (url data)
    (dex:post url :content data :headers '(("Content-Type" . "application/json"))))

(defun fill-form (form data)
    "Return list of pairs (input-name value)."
    (let ((inputs (collect-from form 'input)))
        (loop with input-name for input in inputs
              collect (cons (setf input-name (attribute input 'name))
                            (or (equal-getf data input-name)
                                (attribute input 'value)
                                "")))))

(defun crawl-for-urls (url)
    (let* ((root-node (parse url))
           (hrefs (extract-urls root-node #'same-domain)))
        (loop for href in hrefs
              do (progn (adjoin href hrefs)
                        (crawl-for-urls (prepare-url url href))))
        hrefs))

(defun concat-node-text (node)
    "Return string of text from all of the children nodes."
    (let ((text-list nil))
        (plump:traverse node
                        (lambda (node) (push (plump:text node) text-list))
                        :test #'plump:text-node-p)
        (apply #'concatenate 'string (nreverse text-list))))         
        
(defun download-file (url filename)
    (let ((response (safe-get url)))
        (with-open-file (stream filename :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (when response 
                (loop for byte across response
                      do (write-byte byte stream))))))

(defun download-page (url filename)
    (let ((response (safe-get url)))
        (with-open-file (stream filename :direction :output :if-exists :supersede)
            (when response 
                (write-string response stream)))))

(defun download-all-images (url dir)
    (let ((images (collect-from (parse url) 'img 'src)))
        (setf dir (namestring (ensure-directories-exist (pathname-as-directory dir))))
        (loop for image in images
              when image
                do (handler-case
                       (let ((filename (concatenate 'string dir (aref (get-last image) 1))))
                           (download-file image filename))
                     (error (e) (format t "~&Error while downloading image [~a]~%~a~%" image e))))))

(defun nodes-to-string (list)
    "Return selector string from list of symbols."
    (if (consp list)
        (format nil "~{~a~^ ~}" list)
        (string list)))

(defun attribute (node attr)
    "Return attribute from node."
    (plump:attribute node (string attr)))

(defun collect-from (parent-node selectors &key attr test)
    "Return list of nodes or attributes from parrent node."
    (loop for node across (clss:select (nodes-to-string selectors) parent-node)
          when (or (not test) (funcall test node))
          collect (if attr
                      (attribute node attr)
                      node)))

(defun extract-urls (page &optional test)
    (collect-from page 'a :attr 'href :test test))

(defun extract-input-names (page)
    (collect-from page '(form input) :attr 'name))

(defun extract-js-src (page)
    (collect-from page 'script :attr 'src))

(defun extract-forms (page &optional name)
    "Return list of forms from page or single form if
    page has only one form or specified form-name."
    (let ((forms (collect-from page 'form)))
        (cond ((not (cdr forms))
               (car forms))
              (name
               (loop for form in forms
                     when (substp name (attribute form 'name))
                       return form))
              (t forms))))
