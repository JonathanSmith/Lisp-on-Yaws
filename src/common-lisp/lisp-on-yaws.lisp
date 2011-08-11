(in-package "LISP-ON-YAWS")

(defvar *yaws-server-node-name*)
(defvar *cookie-file*)
(defvar *yaws-listener-thread*)
(defvar *pids-hash* (make-hash-table))
(defvar *reg-pids-hash* (make-hash-table))
(defvar *pid* (cleric:make-pid))
(defvar *thread-pool* (thread-pool:make-thread-pool 6))

(defun cleric::find-connected-remote-node (node-name)
  (flet ((node-name= (node-name1 node-name2)
		     (let ((len1 (length node-name1))
			   (len2 (length node-name2)))
		       (if (> len1 len2)
			   (string= (subseq node-name1 0 len2) node-name2)
			 (string= (subseq node-name2 0 len1) node-name1)))))
    (when (symbolp node-name)
      (setf node-name (symbol-name node-name)))

    (find node-name
	  cleric::*remote-nodes*
	  :key #'cleric::remote-node-name
	  :test #'node-name=)))

(defun node-listener-thread (dispatch-function)
  (setf *yaws-listener-thread*
	(bordeaux-threads:make-thread 
	 (lambda ()
	   (do ()
	       (NIL)
	     (let ((messages (cleric:receive-node-messages)))
	       (when messages (mapcar dispatch-function messages))))))))

(defmethod hash-dispatch ((control-message cleric:reg-send))
  (let* ((to-name (cleric:to-name control-message))
	 (fnlambda (gethash to-name *reg-pids-hash*)))
    (when fnlambda 
      (thread-pool:add-to-pool 
       *thread-pool*
       (lambda ()
	 (funcall fnlambda control-message))))))

(defmethod hash-dispatch ((control-message cleric:send))
  (let* ((to-pid (cleric:to-pid control-message))
	 (fnlambda (gethash to-pid *pids-hash*)))
    (when fnlambda 
      (thread-pool:add-to-pool
       *thread-pool*
       (funcall fnlambda control-message)))))

(defun set-hash-dispatch (fn)
  (let ((pid (cleric:make-pid)))
    (setf (gethash pid *pids-hash*) fn)
    pid))

(defun init-server-connection ()
  (let ((yaws-server (cleric-epmd:lookup-node "yaws" *yaws-server-node-name*)))
    (let ((cookie (with-open-file (stream *cookie-file* :direction :input)
		    (read-line stream))))
      (cleric:remote-node-connect yaws-server cookie)))
  (thread-pool:start-pool *thread-pool*)
  (node-listener-thread #'hash-dispatch))

(defun register (string fn)
  (setf (gethash (intern (string-upcase string) "KEYWORD") *reg-pids-hash* ) fn))

(defvar +file-atom+ (cleric:make-atom "file"))

(defun resource-formatter (resource)
  (concatenate 'string 
	       (apply #'concatenate 'string 
		      (format nil "(~a ~a(" (first resource) (second resource))
		      (mapcar (lambda (pathname-part) 
				(if (symbolp pathname-part) 
				    (format nil " ~a" pathname-part)
				    (format nil " ~s" pathname-part))) (third resource)))
	       "))"))

(defun write-module-string (module-name node-name &rest resources)
  (let ((header (format nil "(defmodule ~a (export (out 1)))" module-name))
	(include (format nil "(include-file \"include/yaws_api.lfe\")" ))
	(gen-out (format nil "(gen_resources ~a ~a)" node-name (mapcar #'resource-formatter resources))))
    (format t "~s~%" resources)
    (format nil "~a~%~a~%~a~%" header include gen-out)))

(defun send-file-for-compilation (module-name file-data)
  (cleric:reg-send *pid* "lfe_compiler" "yaws" 
		   (cleric:tuple +file-atom+  module-name file-data)))

(defun send-static-page (path filename data)
  (cleric:reg-send *pid* "write_page" "yaws" (cleric:tuple path filename data)))

(defvar +update_appmod+ (cleric:make-atom "update_appmod"))

(defun add-appmod (appmod)
  (cleric:reg-send *pid* "conf_control" "yaws"
		   (cleric:tuple +update_appmod+ appmod)))

(defvar *elements*)
(defvar *type*)
(defvar *dest*)
(defvar *ref*)
(defvar *query*)
(defvar *reply-type*)
(defvar *content-type*)

(defun reply (value &optional (reply-type *reply-type*) (mime-type *content-type*))
  (if (eql reply-type :|content|)
      (cleric:send *dest* (cleric:tuple *ref* (cleric:tuple reply-type mime-type value)))
      (cleric:send *dest* (cleric:tuple *ref* (cleric:tuple reply-type value)))))

(defmacro easy-handler ((&rest args) (&optional (default-reply-type :|html|)
						(default-content-type nil)) &body body)
  (let ((control-message (gensym "control-message"))
	(len (gensym "len")))
    `(lambda (,control-message)
       (let* ((*elements* (cleric:elements (cleric:message ,control-message)))
	      (*type* (elt *elements* 0))
	      (*dest* (elt *elements* 1))
	      (*ref* (elt *elements* 2))
	      (*query* (elt *elements* 3))
	      (*reply-type* ,default-reply-type)
	      (*content-type* ,default-content-type)
	      ,@(if args 
		    `((,len (length *elements*)))
		    nil)
	      ,@(let ((i 4))
		     (mapcar #'(lambda (arg) 
				 (prog1 `(,arg (when (> ,len ,i)  (map 'string #'code-char (elt *elements* ,i))))
				   (incf i)))
			     args)))
	   ,@body))))

(defmacro static-page-generator (((path-var filename-var) &rest args) &body body)
  (let ((page-result (gensym)))
    `(lambda (,path-var ,filename-var ,@args)
       (let ((,page-result (progn ,@body)))
	 (lisp-on-yaws:send-static-page ,path-var ,filename-var ,page-result)))))

(defun parse-query (query)
  (mapcar (lambda (tuple)
	      (map 'list (lambda (x) (if (listp x) (map 'string #'code-char x) ""))
		   (cleric:elements tuple)))
	  query))

(defun parse-multipart-query (query)
  (mapcar (lambda (tuple) 
	    (let* ((elements (cleric:elements tuple))
		   (first-element (elt elements 0))
		   (second-element (elt elements 1))
		   (first-element-string (map 'string #'code-char first-element)))
	      
	      (if (listp second-element)
		  (list first-element-string
			(map 'string #'code-char second-element))
		  (let* ((elements (cleric:elements second-element))
			 (element-a (elt elements 0))
			 (element-b (elt elements 1)))
		    (list first-element-string 
			  (map 'string #'code-char element-a) 
			  (cleric::bytes element-b)))))) query))

(defparameter *appmods* (make-hash-table :test #'equalp))

(defun add-appmod-resource (name resource)
  (let ((mod-description (gethash name *appmods* nil)))
    (if mod-description
	(let* ((component-resource (assoc resource mod-description :test #'equalp))
	       (mod-description2 (if component-resource
				     (remove component-resource mod-description :test #'equalp)
				     mod-description)))
	  (setf (gethash name *appmods*) (push resource mod-description2)))
	(setf (gethash name *appmods*) (list resource)))))

(defun appmod-path-name-gen (appmod type path)
  (reduce (lambda (x y) (format nil "~a-~a" x y))
	  (cons (string-upcase (if (stringp type) type (symbol-name type)))
		(cons (string-upcase (if (stringp appmod) appmod (symbol-name appmod)))
		      (mapcar (lambda (p) 
				(string-upcase (cond ((stringp p) p)
						     ((symbolp p) (symbol-name p)))))
			      path)))))

(defmacro defhandler ((mod type path) return-type &body body)
  (let* ((appname (appmod-path-name-gen mod type path))
	 (args (remove-if-not #'symbolp path))
	 (module-resource (list (make-symbol appname) (make-symbol (symbol-name type)) 
				(mapcar (lambda (pathpart)
					  (if (symbolp pathpart)
					      (make-symbol (symbol-name pathpart))
					      pathpart)) path))))

    `(progn
       (lisp-on-yaws:add-appmod-resource ,(if (symbolp mod) (string-downcase (symbol-name mod)) mod) ',module-resource)
       (lisp-on-yaws:register ,appname
			      (easy-handler ,args ,return-type ,@body)))))
  
(defun generate-appmods ()
  (maphash (lambda (mod resources)
	     (let ((mod-string (apply #'write-module-string mod (cleric:this-node) resources)))
	       (send-file-for-compilation mod mod-string)
	       (add-appmod (cleric:tuple mod (cleric:make-atom mod)))))
	   *appmods*))