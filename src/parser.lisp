(in-package :cl-user)
(defpackage stepster.parser
  (:use :cl)
  (:import-from	:stepster.urlworks
   				:same-domain
                :join-with-main
   				:prepare-url
                :make-arguments-string
   :get-last)
  (:import-from :stepster.utils
   :print-error
                :substp
   :regex-group
   :pathname-as-directory)
  (:export
   :parse-url
   :get-root-node
   :attribute
   :collect-from
   :submit-form
   :download-file
   :download-page
   :download-all-images))

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

(defun submit-form (url form-name &rest data-list &key (default nil))
    (let* ((form (extract-forms (get-root-node url) form-name))
           (action (join-with-main url (attribute form 'action)))
           (method (attribute form 'method))
           (data (fill-form-with form data-list default)))
        (if (string-equal method "post")
            (safe-post action data)
            (safe-get (make-arguments-string action data)))))

(defun fill-form-with (form data default)
    (let ((inputs (collect-from form 'input)))
        (loop for input in inputs
              when (string= (attribute input 'type) "text")
                collect (set-input-data input data default))))

(defun crawl-for-urls (url)
    (let* ((root-node (get-root-node url))
           (hrefs (extract-urls root-node)))
        (loop for href in hrefs
              do (when (same-domain href url)
                     (adjoin href hrefs)
                     (crawl-for-urls (prepare-url url href))))
        hrefs))

(defun concat-node-text (node)
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
    (let ((images (collect-from (get-root-node url) 'img 'src)))
        (setf dir (namestring (ensure-directories-exist (pathname-as-directory dir))))
        (loop for image in images
              when image
                do (handler-case
                       (let ((filename (concatenate 'string dir (aref (get-last image) 1))))
                                         
                           (download image filename))
                     (error (e) (format t "~&Error while downloading image [~a]~%~a~%" image e))))))

(defun get-root-node (url)
    (plump:parse (dex:get url :cookie-jar *cookie-jar*)))

(defun check-status (url)
    (nth-value 1 (dex:get url)))

(defun attribute (node attr)
    (plump:attribute node (string attr)))

(defun set-input-data (input data default)
    (let ((name (attribute input 'name)))
        (if (listp data)
            (loop for pair in data
                  if (substp (car pair) name)
                    do (cons name (cadr pair))
                  else do (cons name default))
            (cons name data))))

(defun collect-from (parent-node selectors &optional (attr nil))
    (loop for node across (clss:select (nodes-to-string selectors) parent-node)
          collect (if attr
                      (attribute node attr)
                      node)))

(defun nodes-to-string (list)
    (if (consp list)
        (string-right-trim " "
               (apply #'concatenate 'string
                      (loop for word in list
                            collect (concatenate 'string (string word) " "))))
        (string list)))

(defun extract-urls (root-node)
    (collect-from root-node 'a 'href))

(defun extract-attributes-names (root-node)
    (collect-from root-node '(form input) 'name))

(defun extract-forms (root-node &optional name)
    (let ((forms (collect-from root-node 'form)))
        (if name
            (loop for form in forms
                  when (substp name (attribute form 'name))
                    return form)
            forms)))

(defun safe-get (url)
    (handler-case (dex:get (prepare-url url)
                           :cookie-jar *cookie-jar*)
      (error (e) (print-error e))))

(defun safe-post (url &rest data)
    (handler-case
        (dex:post url
                  :cookie-jar *cookie-jar*
                  :content data)
      (error (e) (print-error e))))
