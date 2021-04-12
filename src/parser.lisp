(in-package :cl-user)
(defpackage stepster.parser
  (:use :cl)
  (:import-from	:stepster.urlworks
   :same-domain
                :join-with-main))

(in-package :stepster.parser)

(defvar *xss-payloads* '("<svg =\"on" "< leg'"))
(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))
(defparameter *test-url* "")
(defparameter *test-node* (plump:parse (dex:get *test-url*)))

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

(defun extract-urls (root-node)
    (collect-from root-node 'a 'href))

(defun extract-forms (root-node)
    (collect-from root-node 'form))

(defun crawl-for-urls (url &optional (urls nil))
    (let* ((root-node (get-root-node url))
           (hrefs (extract-urls root-node)))
        (loop for href in hrefs
              do (when (same-domain href url)
                     (adjoin href urls)
                     (crawl-for-urls href urls)))
        hrefs))

(defun submit-form (url form d)
    (let ((action (join-with-main url (attribute form 'action)))
          (method (attribute form 'method))
          (data (fill-form-with form d)))
    (dex:post url
              :cookie-jar *cookie-jar*
              :content data)))

(defun fill-form-with (form value)
    (let ((inputs (collect-from form 'input)))
        (loop for input in inputs
              when (string= (attribute input 'type) "text")
                collect (set-input-data input value))))

(defun nodes-to-string (list)
    (if (consp list)
        (string-right-trim " "
               (apply #'concatenate 'string
                      (loop for word in list
                            collect (concatenate 'string (string word) " "))))
        (string list)))

(defun concat-node-text (node)
    (let ((text-list nil))
        (plump:traverse node
                        (lambda (node) (push (plump:text node) text-list))
                        :test #'plump:text-node-p)
        (apply #'concatenate 'string (nreverse text-list))))

