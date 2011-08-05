(defparameter *posts-directory* "~/posts/*.pst")
(defvar *post-headers* nil)
(setf *yaws-server-node-name* "jon-desktop")
(setf *cookie-file* "/home/jon/Dropbox/Lisp-on-Yaws/COOKIE")

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
							   (:b (cl-who:str title)))))))
			      (:h4 (:a :href "/blog/" "New Post"))
			      (:h4 (:a :href "/blog/register/" "Register"))
			      )))))
    (send-static-page "posts" "index.html" index-page)))



(defparameter *salt* "PASSWORD")
(defparameter *password-hash* (make-hash-table :test #'equalp))

(defun obfuscate-password (password)
  (let* ((salted (concatenate 'string *salt* password)))
    (map 'string #'code-char (md5::MD5SUM-SEQUENCE salted))))

(defun add-password (name password)
  (setf (gethash name *password-hash*) (obfuscate-password password)))

(defun check-password (name password)
  (string= (gethash name *password-hash*) (obfuscate-password password)))

(defhandler (blog get ()) (:|html|)
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
				 :br
				 (:textarea :row "6" :cols "60" :name "post")
				 :br
				 (:input :type "submit" :value "Submit")))))))

(defhandler (blog post ()) (:|html|)
  (let* ((q (parse-query *query*))
	 (author (second (assoc "author" q :test #'string=)))
	 (password (second (assoc "password" q :test #'string=)))
	 (title (second (assoc "title" q :test #'string=)))
	 (post (second (assoc "post" q :test #'string=))))
    (if (and (and author password title post) (check-password author password))
	(let ((pst-file (generate-post-pst-file title author post)))
	  (generate-post-from-file pst-file)
	  (generate-index)
	  (reply "/posts/index.html" :|redirect|))
	(reply "/index.html" :|redirect|))))

(defhandler (blog get ("register")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:title "Registration")
		  (:body (:B "Register to Post")
			 (:form :action "/blog/register" :method "POST"
				"Author"
				:br
				(:input :type "text" :name "author")
				:br
				"Password"
				:br
				(:input :type "password" :name "password")
				:br
				"Password Again"
				:br
				(:input :type "password" :name "password2")
				:br
				(:input :type "submit" :value "Submit")))))))

(defhandler (blog post ("register")) (:|html|)
  (let*  ((q (parse-query *query*))
	  (author (second (assoc "author" q :test #'string=)))
	  (password (second (assoc "password" q :test #'string=)))
	  (password2 (second (assoc "password2" q :test #'string=))))
    (cond 
      ((or (gethash author *password-hash*) (< (length author) 3))
       (reply (cl-who:with-html-output-to-string (var)
		(:html (:body (:B "Name already taken or name must be at least 3 characters")
			      :br
			      (:b (:a :href "/blog/register"
				      "Try Again")))))))
      ((string= password password2)
       (add-password author password)
       (reply "/blog" :|redirect|))
      (T (reply (cl-who:with-html-output-to-string (var)
		  (:html (:body (:B "Passwords do not match")
				:br
				(:b (:a :href "/blog/register"
					"Try Again"))))))))))


(defun blog-main ()
  (init-server-connection)
  (setf *post-headers* nil)
  (let ((posts (directory *posts-directory*)))
    (dolist (post posts)
      (generate-post-from-file post))
    (setf *post-headers* (sort *post-headers* #'> :key #'first))
    (generate-index))
  (generate-appmods))