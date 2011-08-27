(in-package "BLOG")

(defparameter *posts-directory* (pathname (concatenate 'string (directory-namestring (truename ".")) "/posts/*.pst")))
(defvar *post-headers* nil)
(setf *yaws-server-node-name* "jon-VirtualBox")
(setf *cookie-file* "/home/jon/Lisp-On-Yaws/COOKIE")

(defun timestamp ()
  (multiple-value-bind (second minute hour date month year)  
      (decode-universal-time (get-universal-time))
    (format nil "~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun uuid-string (&optional (uuid (uuid:make-v4-uuid)))
  (with-open-stream (s (make-string-output-stream))
    (uuid:print-bytes s uuid)
    (get-output-stream-string s)))

(defun generate-post-html (universal-time author title body)
  (multiple-value-bind (second minute hour date month year)  (decode-universal-time universal-time)
    (declare (ignore second))
    (let* ((body-string (let ((s (make-string-output-stream))) 
			  (cl-markdown:markdown body :stream s)
			  (get-output-stream-string s)))
	   (date-string (format nil "At ~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d" year month date hour minute))
	   (page (cl-who:with-html-output-to-string (var)
		   (:h2 
		    (:a :href 
			(format nil "/posts/~a.html" universal-time)
			(cl-who:str title)))
		   (cl-who:str body-string)
		   (:h4 (cl-who:str author))
		   (:h4 (cl-who:str date-string)))))
      (send-static-page "posts" (format nil "~a.html" universal-time) page))))

(defun read-body (stream)
  (let* ((len (file-length stream))
	 (pos (file-position stream))
	 (body-string (make-string (- len pos))))
    (read-sequence body-string stream)
    body-string))

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
	   (format stream "~a~%~a~%~a~%" time author title)))
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

(defvar *most-recent-post* "")

(defun generate-index ()
  (when (first *post-headers*)
    (setf *most-recent-post* (format nil "~a" (first (first *post-headers*)))))
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
			 (named-link var link "div#blog" title)))))))
	   (:input :type "hidden" :id "latest" :name "latest" :value (cl-who:str *most-recent-post*)))))
    (send-static-page "posts" "index.html" index-page)
    
    nil))

(defparameter *salt* "PASSWORD")
(defvar *password-hash* (make-hash-table :test #'equalp))

(defun obfuscate-password (password)
  (let* ((salted (concatenate 'string *salt* password)))
    (map 'string #'code-char (md5::MD5SUM-SEQUENCE salted))))

(defun add-password (name password)
  (setf (gethash name *password-hash*) (obfuscate-password password)))

(defun check-password (name password)
  (string= (gethash name *password-hash*) (obfuscate-password password)))

(ps:defpsmacro js-link (link div-id)
  `($.get ,link
	  (ps:create)
	  (lambda (data)
	    (ps:chain ($ ,div-id) 
		      (html data)))))

(ps:defpsmacro defpostfn (name path 
			  (args1 &body body1) 
			  (args2 &body body2))
  (let ((strings (mapcar #'(lambda (symbol) (string-downcase (symbol-name symbol))) path)))
    (let ((path-name (reduce (lambda (name1 name2)
			       (format nil "~a~a/" name1 name2)) 
			     (cons (format nil "/~a/" (car strings))
				   (cdr strings))))
	  (post-result (gensym)))
      `(defun ,name (,@args1)
	 (let ((,post-result (progn ,@body1)))
	   ($.post 
	    ,path-name
	    ,post-result
	    (lambda (,@args2) ,@body2)))))))
 
(defun named-link (stream link div-id name)
  (cl-who:with-html-output (stream)
    (:a :href "#" :onclick  
	(ps:ps-inline* `(js-link ,link ,div-id))
	(cl-who:str name))))

#|(defhandler (blog get ("ymacs"))(:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:head (:title "Ymacs")
			 (:link :rel "stylesheet" :type "text/css" :href "/dl/css/default.css")
			 (:link :rel "stylesheet" :type "text/css" :href "/test.css")
			 (:link :rel "stylesheet" :type "text/css" :href "/ymacs/css/ymacs.css"))

		  (:center :style "margin-top: 10em" :id "x-loading"(:h1 (:tt "Loading")))
		  (:script "window.Dynarc_base_Url = \"/dl\"; window.YMACS_SRC_PATH = \"/ymacs/js/\"")
		  (:script :src "/dl/js/thelib.js")
		  (:script :src "/ymacs/js/ymacs.js")
		  (:div :style "display: none"
			(:div :id "browse-warning" :style "padding: 1em; width 20em;"
			      (:b "ymacs disclaimer blah")))
		  (:script :src "/test2.js")))))|#

(defhandler (blog get ("last_post")) (:|content| "application/json")
    (reply *most-recent-post*))

(defhandler (blog get ()) (:|html|)
  (reply 
   (cl-who:with-html-output-to-string (var)
     (:html (:head (:title "Jon Feed")
		   (:link :rel "stylesheet" :href "/blog.css"))
	    (:body 
	     (:h1 "JonFeed")
	     (:h4 "For all your Jon News")
	     (:div :id "user")
	     (:div :id "index")
	     (:div :id "blog")
	     (:script :src "/jquery.min.js")
	     (let ((link (format nil "/posts/~a.html" *most-recent-post*)))
	       (cl-who:htm
		(:script :type "text/javascript"
			 (cl-who:str (ps:ps* `(defun get-init-post ()
						(js-link ,link "div#blog"))))
			 (cl-who:str 
			  (ps:ps 
			    (ps:chain 
			     ($ document) 
			     (ready
			      (lambda ()
				(get-init-post)
				(check-last-post)
				(poll-index))))

			    (defun check-last-post ()
			      ($.get "/blog/last_post"  
				     (ps:create)
				     (lambda (server-id)
				       (let ((this-id (ps:chain ($ "input#latest") (val))))
					 (unless (equal this-id server-id)
					   ($.get "/posts/index.html"
						  (ps:create)
						  (lambda (data)
						    (ps:chain 
						     ($ "div#index")
						     (html data)))))))))
			     
			    (defun poll-index ()
			      (ps:var timer (set-interval "checkLastPost()" 30000))))))
		(:div :id "footer"
		      (named-link var "/blog/register/" "div#blog" "Register") :br
		      (named-link var "/blog/post/" "div#blog" "Add A Post") :br
		      (named-link var "/blog/chat/" "div#blog" "Chat") :br
		      (named-link var "/blog/login/" "div#blog" "Login"))
		(:input :type "hidden" :id "session-id" :name "session-id"))))))))

(defhandler (blog get ("post")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html 
	    (:title "A Blog") 
	    (:body  
	     (:script :type "text/javascript"
		      (cl-who:str
		       (ps:ps (defpostfn make-post (blog post)
				((session-id title text)
				 (ps:create "session-id" session-id
					    "title" title
					    "post" text))
				((data textstatus qxhr)
				 (js-link "/blog/" "div#blog")
				 (js-link "/posts/index.html" "div#index"))))))

	     (:B "Not Much Here")		   
	     :br
	     "Title"
	     (:input :type "text" :name "title" :id "title")
	     :br
	     "Text"
	     :br
	     (:textarea :row "6" :cols "60" :name "post-text" :id "post-text")
	     :br
	     (:input :type "submit" :value "Submit" :onclick
		     (ps:ps-inline 
		      (make-post (ps:chain ($ "input#session-id")
					   (val))
				 (ps:chain ($ "input#title")
					   (val))
				 (ps:chain ($ "textarea#post-text")
					   (val))))))))))

(defhandler (blog post ("post")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (title (second (assoc "title" q :test #'string=)))
	 (post (second (assoc "post" q :test #'string=))))
    (let ((login-info (check-login session-id)))
      (if (and login-info title post)	
	  (let* ((author (login-info-author login-info))
		 (pst-file (generate-post-pst-file title author post)))
	    (generate-post-from-file pst-file)
	    (generate-index)
	    (reply "post added"))
	  (reply "post error")))))

(defhandler (blog get ("register")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:title "Registration")
		  (:body (:B "Register to Post Comments")
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
				"Authorization Code"
				:br
				(:input :type "text" :name "auth")
				:br
				(:input :type "submit" :value "Submit")))))))

(defvar *auth-code* "lisp rocks")

(defhandler (blog post ("register")) (:|html|)
  (let*  ((q (parse-query *query*))
	  (auth-code (second (assoc "auth" q :test #'string=)))
	  (author (second (assoc "author" q :test #'string=)))
	  (password (second (assoc "password" q :test #'string=)))
	  (password2 (second (assoc "password2" q :test #'string=)))
	  (auth-code-valid (and auth-code (string= auth-code *auth-code*))))
    (cond 
      ((or (gethash author *password-hash*) (< (length author) 3))
       (reply (cl-who:with-html-output-to-string (var)
		(:html (:body (:B "Name already taken or name must be at least 3 characters")
			      :br (:b (:a :href "/blog/register" "Try Again")))))))
      ((and (string= password password2) auth-code-valid)
       (add-password author password)
       (reply "/blog" :|redirect|))
      (T (reply (cl-who:with-html-output-to-string (var)
		  (:html (:body (:B "Passwords do not match")
				:br (:b (:a :href "/blog/register" "Try Again"))))))))))

(defhandler (blog get ("login")) (:|html|)
  (reply (cl-who:with-html-output-to-string (val)
	   (:body 
	    (:script 
	     :type "text/javascript" 
	     (cl-who:str
	      (ps:ps 
		(defpostfn login (blog login) 
		    ((user-id password) 
		     (ps:create "author" user-id "password" password))
		  ((data texstatus qxhr)
		   (ps:chain ($ "input#session-id") (val data))
		   (ps:chain ($ "div#user") (html "Logged In"))
		   ;(alert (ps:chain ($ "input#session-id") (val)))
		   )))))
	    "Login Name"
	    :br
	    (:input :type "text" :id "author" :name "author") :br
	    "Password"
	    :br
	    (:input :type "password" :id "password" :name "password") :br
	    (:input :type "submit" :value "Login" :onclick 
		    (ps:ps-inline (login
				   (ps:chain ($ "input#author")
					     (val))
				   (ps:chain ($ "input#password")
					     (val)))))))))

(defvar *logged-in-hash* (make-hash-table :test 'equalp))

(defstruct login-info
  (author)
  (uuid)
  (timestamp))

(defun create-login (author password)
  (when (check-password author password)
    (let* ((uuid (uuid-string))
	   (login (make-login-info :author author :uuid uuid :timestamp (get-universal-time))))

      (setf (gethash uuid *logged-in-hash*) login)

      uuid)))

(defvar *login-timeout* 
  (encode-universal-time 1 20 2 1 1 1))

(defun check-timeout (uuid)
  (> (- (login-info-timestamp uuid) (get-universal-time))
     *login-timeout*)
  t)

(defun check-login (uuid)
  (let ((login-info (gethash uuid *logged-in-hash*)))
    (when (and login-info (check-timeout login-info))
      login-info)))

(defhandler (blog post ("login")) (:|html|)
  (let ((q (parse-query *query*)))
    (let ((author (second (assoc "author" q  :test #'string=)))
	  (password (second (assoc "password" q :test #'string=))))
      (let ((uuid (create-login author password)))
	(if uuid 
	    (reply uuid)
	    (reply ""))))))
	 

(let ((chat-mutex (sb-thread:make-mutex))
      (chat-position 0)
      (chat-length 20)
      (chat-array (make-array 20 :initial-element nil))
      (chat-reply-list nil))

  (defun get-chat-text ()
    (sb-thread:with-recursive-lock (chat-mutex)
      (let ((position (mod chat-position chat-length))
	    (text-list (list)))
	(dotimes (i chat-length)
	  (push (aref chat-array (mod (+ position i) chat-length)) text-list))
	(apply #'concatenate 'string (nreverse text-list)))))

  (defun log-chat-text (string) string)

  (defun set-chat-text (string)
    (sb-thread:with-recursive-lock (chat-mutex)
      (let ((current-position (mod chat-position chat-length)))
	(when (aref chat-array current-position)
	  (log-chat-text (aref chat-array current-position)))
	(setf (aref chat-array current-position) string)
	(incf chat-position)))
    (reply-chat))

  (defun queue-request () 
    (sb-thread:with-recursive-lock (chat-mutex)
      (push (get-reply-information) chat-reply-list)))




  (defun reply-chat ()
    (sb-thread:with-recursive-lock (chat-mutex)
      (reply-all (get-chat-text) chat-reply-list :|html|)
      (setf chat-reply-list nil)))

  (defun init-chat-reply-thread ()
    (sb-thread:make-thread (lambda () 
			       (do ()
				   (NIL)
				 (sleep 1)
				 (reply-chat))))))

(defhandler (blog get ("chat")) (:|html|)
  (reply (cl-who:with-html-output-to-string (val)
	   (:html (:body 
		   (:script :type "text/javascript" 
			    (cl-who:str
			     (ps:ps
			       (defun chat-loop-init ()
				 ($.get "/blog/chat/instant"
					(ps:create :session-id 
						   (ps:chain ($ "input#session-id")
							     (val)))
					(lambda (data)
					  (ps:chain ($ "div#chatwindow")
						    (html data))
					  (chat-loop))))
			       (defun chat-loop ()
				 ($.get "/blog/chat/wait"
					(ps:create :session-id 
						   (ps:chain ($ "input#session-id")
							     (val)))
					(lambda (data)
					  (ps:chain ($ "div#chatwindow") 
						    (html data))
					  (chat-loop))))

			       (defun key-stroke-update (event)
				 (if (or (= (ps:chain event char-code) 13)
					 (= (ps:chain event key-code) 13))
				     (post)))

			       (defun post () 
				 ($.post
				  "/blog/chat"
				  (ps:create 
				   :message 
				   (ps:chain 
				    ($ "input#message")
				    (val))
				   :session-id
				   (ps:chain 
				    ($ "input#session-id")
				    (val))
				   ))
				 (ps:chain ($ "input#message") (val "")))
			      
			       (ps:chain 
				($ document) 
				(ready
				 (chat-loop-init))))))
		   (:div :id "chatwindow")
		   :br
		   "Message: "
		   (:input 
		    :id "message"
		    :type "text"
		    :name "message"
		    :onkeypress (ps:ps-inline (key-stroke-update event)))
		   :br
		   (:input :type "submit" 
			   :value "Send"
			   :onclick (ps:ps-inline 
				     (post))))))))

(defhandler (blog get ("chat" "wait")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (when (check-login session-id)
      (queue-request))))

(defhandler (blog get ("chat" "instant")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (when (check-login session-id)
      (reply (get-chat-text)))))

(defhandler (blog post ("chat")) (:|html|)
  (reply "")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (message (second (assoc "message" q :test #'string=))))
    (let ((login-info (check-login session-id)))
      (when (and login-info message)
	(let ((name (login-info-author login-info)))
	  (set-chat-text (cl-who:conc
			  (cl-who:escape-string-iso-8859-1 
			   (format nil "~a ~a: ~a" (timestamp) name message))
			  "<br></br>")))))))

(defun blog-main ()
  (init-server-connection)
  (setf *post-headers* nil)
  (let ((posts (directory *posts-directory*)))
    (dolist (post posts)
      (generate-post-from-file post))
    (setf *post-headers* (sort *post-headers* #'> :key #'first))
    (generate-index))
  (generate-appmods))