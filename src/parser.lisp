(in-package :cl-user)
(defpackage stepster.parser
  (:use :cl)
  (:import-from	:stepster.urlworks
   				:same-domain
                :join-with-main
   :prepare-url
                :make-arguments-string
                :get-last))

(in-package :stepster.parser)

(defvar *xss-payloads* '("<svg =\"on" "< leg'"))
(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))
;(defparameter *test-url* "")
;(defparameter *test-node* (plump:parse (dex:get *test-url*)))

(defun get-root-node (url)
    (plump:parse (dex:get url)))

(defun status-code (url)
    (nth-value 1 (dex:get url)))

(defun attribute (node attr)
    (plump:attribute node (string attr)))

(defun set-input-data (input value)
    (cons (attribute input 'name) value))

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

(defun extract-forms (root-node)
    (collect-from root-node 'form))

(defun crawl-for-urls (url)
    (let* ((root-node (get-root-node url))
           (hrefs (extract-urls root-node)))
        (loop for href in hrefs
              do (when (same-domain href url)
                     (adjoin href hrefs)
                     (crawl-for-urls (prepare-url url href))))
        hrefs))

(defun submit-form (url form d)
    (let ((action (join-with-main url (attribute form 'action)))
          (method (attribute form 'method))
          (data (fill-form-with form d)))
        (if (string-equal method "post")
            (dex:post action
              :cookie-jar *cookie-jar*
              :content data)
            (dex:get (make-arguments-string action data)
                     :cookie-jar *cookie-jar*))))

(defun fill-form-with (form value)
    (let ((inputs (collect-from form 'input)))
        (loop for input in inputs
              when (string= (attribute input 'type) "text")
                collect (set-input-data input value))))

(defun concat-node-text (node)
    (let ((text-list nil))
        (plump:traverse node
                        (lambda (node) (push (plump:text node) text-list))
                        :test #'plump:text-node-p)
        (apply #'concatenate 'string (nreverse text-list))))

(defun download-all-images (url dir)
    (let ((images (collect-from (get-root-node url) 'img 'src)))
        (loop for image in images
              when image
                do (handler-case (progn
                                     (let ((filename (concatenate 'string dir (aref (get-last image) 1))))
                                         (download image filename)))
                     (error (e) (format t "~&Error while downloading image [~a]~%~a~%" image e))))))

(defun download (url filename)
    (let ((response (safe-get url)))
        (with-open-file (stream filename :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (when response 
                (loop for byte across response
                      do (write-byte byte stream))))))
        
(defun safe-get (url)
    (handler-case (dex:get url)
      (error (e) (format t "Error:~a~%" e))))
