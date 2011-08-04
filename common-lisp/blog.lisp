(defparameter *posts-directory* "~/posts/*.pst")
(defvar *post-headers* nil)
(setf *yaws-server-node-name* "jon-desktop")
(setf *cookie-file* "/home/jon/Dropbox/Lisp-on-Yaws/COOKIE")

;;header -> (universal-time author title)

(defun generate-post-page (header body)
  (destructuring-bind (universal-time author title) header 
    (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time)
      (declare (ignore second))
      (let* ((body-string (eval body))
	     (date-string (format nil "~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d" year month date hour minute))
	     (page (cl-who:with-html-output-to-string (var)
		     (:html (:head (:title (cl-who:str title)))
			    (:body (:h1 (cl-who:str title))
				   (:h2 (cl-who:str author))
				   (:h2 (cl-who:str date-string))
				   (:p (cl-who:str body-string))
				   :br
				   (:a :href "http://0.0.0.0:8080/posts/index.html"
				       (:b "back to index")))))))
	(send-static-page "posts" (format nil "~a.html" universal-time) page)))))

(defun generate-post (post)
  (with-open-file (post-stream post :direction :input)
    (let ((header (read post-stream nil :eof nil))
	  (body (read post-stream nil :eof nil)))
      (assert (not (or (eql :eof header) (eql :eof :body))))
      (generate-post-page header body)
      (push header *post-headers*))))

(defun generate-index ()
  (let ((index-page (cl-who:with-html-output-to-string (var)
		      (:html (:head (:title "Index"))
			     (:body 
			      (:h1 "Jon Smith Weblog")
			      (loop for header in *post-headers*
				 do (destructuring-bind (time author title) header
				      (declare (ignore author))
				      (cl-who:htm (:h2 (:a :href (format nil "http://0.0.0.0:8080/posts/~a.html" time)
						      (:b (cl-who:str title))))
						  :br))))))))
    (send-static-page "posts" "index.html" index-page)))

(defun blog-main ()
  (setf *post-headers* nil)
  (let ((posts (directory *posts-directory*)))
    (dolist (post posts)
      (generate-post post))
    (setf *post-headers* (sort *post-headers* #'> :key #'first))
    (generate-index)))

(defun parse-query (query)
  (mapcar #'(lambda (tuple)
	      (map 'list #'(lambda (x) (map 'string #'code-char x)) (cleric:elements tuple))) query))

(defun add-blog-appmod ()
  (let ((hello-blog-lfe (write-module-string "blog" (cleric:this-node)
					     '(main GET ())
					     '(main POST ()))))
    (send-file-for-compilation "blog" hello-blog-lfe)
    (add-appmod (cleric:tuple "blog" (cleric:make-atom "blog")))
    (register "main"
	      (easy-handler () (:|html|)
		(format t "~s~%" (list *type* *query*))
		(when (eql *type* :POST)
		  (format t "~s~%" (parse-query *query*)))

		(reply (cl-who:with-html-output-to-string (var)
			 (:html (:title "A Blog") 
				(:body  (:B "Not Much Here")
					(:form :action "/blog" :method "POST"
					       "Author"
					       :br
					       (:input :type "text" :name "author")
					       :br
					       "Password"
					       :br
					       (:input :type "text" :name "password")
					       :br
					       "Title"
					       (:input :type "text" :name "title")
					       :br
					       "Text"
					       (:input :type "text" :name "post")
					       :br
					       (:input :type "submit" :value "Submit"))
					))))))))