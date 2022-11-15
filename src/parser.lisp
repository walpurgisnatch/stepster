(in-package :cl-user)
(defpackage stepster.parser 
  (:use :cl)
  (:import-from	:stepster.urlworks
                :same-domain
                :join-with-main
                :prepare-url
                :make-query-string
                :get-last)
  (:import-from :stepster.json-works
                :getj)
  (:import-from :stepster.utils
                :print-error
                :substp
                :equal-getf
                :pathname-as-directory
                :clist)
  (:export
   :parse
   :attribute
   :collect-from
   :node-with-attr
   :find-by-text
   :concat-node-text
   :submit-form
   :download-file
   :download-page
   :download-all-images
   :fill-form
   :extract-forms
   :extract-urls
   :extract-js-src
   :parse-json
   :get-json
   :post-json
   :safe-get
   :safe-post
   :crawl
   :parse-regex
   :text
   :check-attr
   :prepare-url
   :get-status-code))

(in-package :stepster.parser)


(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))
(defparameter *user-agent-header* '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:96.0) Gecko/20100101 Firefox/96.0")))

(defmacro for-js (page &body body)
  `(let ((srcs (extract-js-src (parse ,page))))
     (loop for src in srcs do
       (let ((js-file (babel:octets-to-string (safe-get src) :encoding :utf-8)))
         (progn ,@body)))))

(defun parse (page &key headers)
  "Return plump root node for given url"
  (handler-case
      (if (stringp page)
          (plump:parse (safe-get page :headers headers))
          page)
    (error (e) nil)))    

(defun safe-get (url &key (headers *user-agent-header*))
  (handler-case (dex:get (prepare-url url)
                         :cookie-jar *cookie-jar*
                         :headers headers)
    (error (e) (print-error e))))

(defun safe-post (url data &key (headers *user-agent-header*))
  (handler-case (dex:post (prepare-url url)
                          :cookie-jar *cookie-jar*
                          :headers headers
                          :content data)
    (error (e) (print-error e))))

(defun get-status-code (url)
  (handler-case (nth-value 1 (dex:get (prepare-url url)
                                      :headers *user-agent-header*))
    (dex:http-request-bad-request ()
      400)
    (dex:http-request-failed (e)
      (dex:response-status e))
    (error (e) e)))

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

(defun crawl (url &optional (res nil))
  "PLEASE FIX ME ТЫ УРОД"
  (handler-case 
      (let* ((root-node (parse url))
             (hrefs (remove-duplicates (remove-if #'(lambda (x) (member x res :test #'equal))
                                                  (mapcar #'(lambda (href) (prepare-url href url))
                                                          (extract-urls root-node #'same-domain url))) :test #'equal)))
        (setf res (append res hrefs))
        (cond ((null hrefs) res)
              (t (loop for href in hrefs do
                (crawl href res)))))
    (error () nil)))

(defun concat-node-text (page)
  "Return string of text from all of the children nodes."
  (let ((node (parse page))
        (text-list nil))
    (plump:traverse node
                    (lambda (node) (push (plump:text node) text-list))
                    :test #'plump:text-node-p)
    (apply #'concatenate 'string (nreverse text-list))))

(defun find-by-text (parent-node text &key attr)
  (let ((result nil))
    (plump:traverse parent-node
                    (lambda (node) (setf result (if attr (attribute (plump:parent node) attr) (plump:parent node))))
                    :test (lambda (node) (and (plump:text-node-p node) (substp text (plump:text node)))))
    result))

(defun parse-regex (url regex)
  (cl-ppcre:all-matches-as-strings regex (concat-node-text url)))

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
  (let ((images (collect-from (parse url) '(img src))))
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

(defun collect-from (parent-node selectors &key attr test test-args)
  "Return list of nodes or attributes from parrent node."
  (handler-case
      (loop for node across (clss:select (nodes-to-string selectors) parent-node)
            with attribute
            unless attr
              when (or (not test) (apply test (clist node test-args)))
                collect node
            else do (null nil)
            else do (setf attribute (attribute node attr))
                 and when (or (not test) (apply test (clist node test-args)))
                       collect attribute)
    (error (e) (progn (print e) nil))))

(defun node-with-attr (parent-node selector attr val)
  (handler-case 
      (loop for node across (clss:select (nodes-to-string selector) parent-node)
            when (equal (attribute node attr) val)
              return node)
    (error (e) (progn (print e) nil))))

(defun check-attr (attribute)
  #'(lambda (node attr)
      (equal (attribute node attribute) attr)))

(defun text (page selectors &key test test-args)
  (handler-case
      (let ((node (collect-from page selectors :test test :test-args test-args)))
        (plump:text (if (consp node) (car node) node)))
    (error (e) (progn (print e) nil))))

(defun extract-urls (page &optional test arg)
  (collect-from page 'a :attr 'href :test test :test-args arg))

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
