(defparameter *posts-directory* "~/posts/*.pst")
(defvar *post-headers* nil)
(setf *yaws-server-node-name* "jon-VirtualBox")
(setf *cookie-file* "/home/jon/Lisp-On-Yaws/COOKIE")

;;header -> (universal-time author title)

(defun generate-post-html (header body)
  (destructuring-bind (universal-time author title) header 
    (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time)
      (declare (ignore second))
      (let* ((body-string (if (listp body) (eval body) body))
	     (date-string (format nil "At ~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d" year month date hour minute))
	     (page (cl-who:with-html-output-to-string (var)
		     (:html (:head (:title (cl-who:str title)))
			    (:body (:h1 (cl-who:str title))
				   
				   (:p (cl-who:str body-string))
				   (:h4 (cl-who:str author))
				   (:h4 (cl-who:str date-string))
				   :br
				   (:a :href "/posts/index.html"
				       (:b "back to index")))))))
	(send-static-page "posts" (format nil "~a.html" universal-time) page)))))

(defun generate-post-from-file (post)
  (with-open-file (post-stream post :direction :input)
    (let ((header (read post-stream nil :eof nil))
	  (body (read post-stream nil :eof nil)))
      (assert (not (or (eql :eof header) (eql :eof :body))))
      (generate-post-html header body)
      (push header *post-headers*))))


(defun generate-post-pst-file (title author lines)
  (flet ((write-header (stream title author time)
	   (format stream "~s~%" (list time author title))))

    (let* ((time (get-universal-time))
	   (post-id (format nil "~s.pst" time))
	   (post-path (merge-pathnames post-id *posts-directory*))
	   post-lines)

      (with-open-stream (str-stream (make-string-input-stream lines))
	(do ((line (read-line str-stream nil :eof nil) (read-line str-stream nil :eof nil)))
	    ((eql line :eof))
	  (push (list :p line) post-lines)))

      (with-open-file (stream post-path
			      :direction :output 
			      :if-exists :supersede 
			      :if-does-not-exist :create)
	(write-header stream title author time)
	(format stream "~s~%" `(cl-who:with-html-output-to-string (var) ,@post-lines)))
      post-path)))

(defun generate-index ()
  (let ((index-page (cl-who:with-html-output-to-string (var)
		      (:html (:head (:title "Index"))
			     (:body 
			      (:h1 "Jon Smith Weblog")
			      (loop for header in *post-headers*
				 do (destructuring-bind (time author title) header
				      (declare (ignore author))
				      (cl-who:htm (:h2 (:a :href (format nil "/posts/~a.html" time)
							   (:b (cl-who:str title))))
						  :br)))
			      (:h4 (:a :href "/blog/"
				       "New Post" ))
			      )))))
    (send-static-page "posts" "index.html" index-page)))

(defun blog-main ()
  (setf *post-headers* nil)
  (let ((posts (directory *posts-directory*)))
    (dolist (post posts)
      (generate-post-from-file post))
    (setf *post-headers* (sort *post-headers* #'> :key #'first))
    (generate-index)))

(defun parse-query (query)
  (mapcar #'(lambda (tuple)
	      (map 'list #'(lambda (x) (map 'string #'code-char x)) (cleric:elements tuple))) query))

(defparameter *salt* "PASSWORD")
(defparameter *password-hash* (make-hash-table :test #'equalp))

(defun obfuscate-password (password)
  (let* ((salted (concatenate 'string *salt* password))
	 (map 'string #'code-char (md5::MD5SUM-SEQUENCE salted)))))

(defun add-password (name password)
  (setf (gethash name *password-hash*) (obfuscate-password password)))

(defun check-password (name password)
  (equalp (gethash name *password-hash*) (obfuscate-pasword password)))

(defun add-blog-appmod ()
  (let ((hello-blog-lfe (write-module-string "blog" (cleric:this-node)
					     '(main GET ())
					     '(main POST ()))))
    (send-file-for-compilation "blog" hello-blog-lfe)
    (add-appmod (cleric:tuple "blog" (cleric:make-atom "blog")))
    (register "main"
	      (easy-handler () (:|html|)
		(if (eql *type* :POST)
		    (let* ((q (parse-query *query*))
			   (author (second (assoc "author" q :test #'string=)))
			   (password (second (assoc "password" q :test #'string=)))
			   (title (second (assoc "title" q :test #'string=)))
			   (post (second (assoc "post" q :test #'string=))))
		      (when (and (and author password title post) (check-password author password))
			(let ((pst-file (generate-post-pst-file title author post)))
			  (generate-post-from-file pst-file)
			  (generate-index))
			(reply "/posts/index.html" :|redirect|)))
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
					       (:input :type "password" :name "password")
					       :br
					       "Title"
					       (:input :type "text" :name "title")
					       :br
					       "Text"
					       (:textarea :row "6" :cols "80" :name "post")
					       :br
					       (:input :type "submit" :value "Submit")))))))))))