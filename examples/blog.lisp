(defparameter *posts-directory* "~/posts/*.pst")
(defvar *post-headers* nil)
(setf *yaws-server-node-name* "jon-desktop")
(setf *cookie-file* "/home/jon/Dropbox/Lisp-on-Yaws/COOKIE")


(defun generate-post-html (universal-time author title body)
  (multiple-value-bind (second minute hour date month year)  (decode-universal-time universal-time)
    (declare (ignore second))
    (let* ((body-string (eval `(cl-who:with-html-output-to-string (var) ,@body)))
	   (date-string (format nil "At ~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d" year month date hour minute))
	   (page (cl-who:with-html-output-to-string (var)
		   (:h2 (:a :href (format nil "/posts/~a.html" universal-time) (cl-who:str title)))
		   (cl-who:str body-string)
		   (:h4 (cl-who:str author))
		   (:h4 (cl-who:str date-string)))))
      (send-static-page "posts" (format nil "~a.html" universal-time) page))))

(defun read-body (stream &aux (post-lines nil))
  (do ((line (read-line stream nil :eof nil) (read-line stream nil :eof nil)))
      ((eql line :eof))
    (push (list :p line) post-lines))
  post-lines)

(defun generate-post-from-file (post)
  (with-open-file (post-stream post :direction :input)
    (let* ((universal-time (read-line post-stream nil :eof nil))
	  (author (read-line post-stream nil :eof nil))
	  (title (read-line post-stream nil :eof nil))
	  (time (parse-integer universal-time))
	  (body (read-body post-stream)))
      (generate-post-html time author title body)
      (push (list time  author title) *post-headers*))))

(defun generate-post-pst-file (title author lines)
  (flet ((write-header (stream title author time)
	   (format stream "~a~% ~a~% ~a~%" time author title)))
    (let* ((time (get-universal-time))
	   (post-id (format nil "~s.pst" time))
	   (post-path (merge-pathnames post-id *posts-directory*)))

      (with-open-file (stream post-path
			      :direction :output 
			      :if-exists :supersede 
			      :if-does-not-exist :create)
	(write-header stream title author time)
	(format stream "~a" lines))
      post-path)))

(defun generate-index ()
  (let ((index-page 
	 (cl-who:with-html-output-to-string (var)
	   (:ul :class "navbar"
		(loop for header in *post-headers*
		   do 
		   (destructuring-bind (time author title) header
		     (declare (ignore author))
		     (let ((link (format nil "/posts/~a.html" time)))
		       (cl-who:htm 
			(:li 
			 (:a :href "#" :onclick  
			     (ps:ps-inline*
			      `($.get ,link 
				      (ps:create)
				      (lambda (data)
					(ps:chain ($ "div#blog") 
						  (html data)))) #\")
			     (:b (cl-who:str title)))))))))
	   (destructuring-bind (time author title) (first *post-headers*)
	     (declare (ignore author title))
	     (let ((link (format nil "/posts/~a.html" time)))
	       (cl-who:htm 
		(:script 
		 (cl-who:str 
		  (ps:ps-inline*
		   `($.get ,link 
			   (ps:create)
			   (lambda (data)
			     (ps:chain ($ "div#blog") 
				       (html data)))) #\")))))))))
    
    (send-static-page "posts" "index.html" index-page)))

(defparameter *salt* "PASSWORD")
(defvar *password-hash* (make-hash-table :test #'equalp))

(defun obfuscate-password (password)
  (let* ((salted (concatenate 'string *salt* password)))
    (map 'string #'code-char (md5::MD5SUM-SEQUENCE salted))))

(defun add-password (name password)
  (setf (gethash name *password-hash*) (obfuscate-password password)))

(defun check-password (name password)
  (string= (gethash name *password-hash*) (obfuscate-password password)))

(defhandler (blog get ("ymacs"))(:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:head (:title "Ymacs")
			 (:link :rel "stylesheet" :type "text/css" :href "/dl/css/default.css")
			 (:link :rel "stylesheet" :type "text/css" :href "/test.css")
			 (:link :rel "stylesheet" :type "text/css" :href "/ymacs/css/ymacs.css"))
		  (:body 
		   (:center :style "margin-top: 10em" :id "x-loading"(:h1 (:tt "Loading")))
		   (:script "window.Dynarc_base_Url = \"/dl\"; window.YMACS_SRC_PATH = \"/ymacs/js/\"")
		   (:script :src "/dl/js/thelib.js")
		   (:script :src "/ymacs/js/ymacs.js")
		   (:div :style "display: none"
			 (:div :id "browse-warning" :style "padding: 1em; width 20em;"
			       (:b "ymacs disclaimer blah")))
		   (:script :src "/test2.js"))))))

(defhandler (blog get ()) (:|html|)
  (reply 
   (cl-who:with-html-output-to-string (var)
     (:html (:head (:title "Jon's Blog")
		   (:link :rel "stylesheet" :href "/blog.css"))
	    (:body 
	     (:h1 "Jon's Web Log")
	     (:div :id "index")
	     (:div :id "blog")
	     (:script :src "/jquery.min.js")
	     (:script :type "text/javascript"
		      (cl-who:str 
		       (ps:ps 
			 (ps:chain 
			  ($ document) 
			  (ready
			   (lambda ()
			     ($.get "/posts/index.html"
				    (ps:create)
				    (lambda (data)
				      (ps:chain ($ "div#index") 
						(html data)))))))))))))))

(defhandler (blog get ("post")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html 
	    (:title "A Blog") 
	    (:body  (:B "Not Much Here")
		    (:form :action "/blog/post" :method "POST"
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

(defhandler (blog post ("post")) (:|html|)
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
	(reply "/posts/index.html" :|redirect|))))

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
			      :br (:b (:a :href "/blog/register" "Try Again")))))))
      ((string= password password2)
       (add-password author password)
       (reply "/blog" :|redirect|))
      (T (reply (cl-who:with-html-output-to-string (var)
		  (:html (:body (:B "Passwords do not match")
				:br (:b (:a :href "/blog/register" "Try Again"))))))))))

(defhandler (blog post ("print")) (:|html|)
  (reply "")
  (format t "~s~%" *query*))

(defhandler (blog get ("file")) (:|html|)
  (reply 
   (cl-who:with-html-output-to-string (var)
     (:html (:body (:form :action "/blog/file"
			  :enctype "multipart/form-data"
			  :method "post"
			  (:p "What is your name?" (:input :type "text" :name "name_of_sender")
			      "What files are you sending?" (:input :type "file" :name "name_of_files"))
			  (:input :type "submit" :value "Submit")))))))

(defhandler (blog post-multipart ("file")) (:|html|)
  (reply "done!")
  (format t "~s~%" *query*)
  (format t "~s~%" (parse-multipart-query *query*)))

(defun blog-main ()
  (init-server-connection)
  (setf *post-headers* nil)
  (let ((posts (directory *posts-directory*)))
    (dolist (post posts)
      (generate-post-from-file post))
    (setf *post-headers* (sort *post-headers* #'> :key #'first))
    (generate-index))
  (generate-appmods))