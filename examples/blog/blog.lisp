(in-package "BLOG")

(defparameter *posts-directory* 
  (pathname (concatenate 'string (directory-namestring (truename ".")) "/posts/*.pst")))

(defvar *password-ns* "PW")
(defvar *settings-ns* "SET")
(defvar *pst-ns* "PST")
(defvar *pst-title* "PST-TITLE")
(defvar *pst-idx* "PSTDX")
(defvar *login-cookie-ns* "LC")
(defvar *chat-ns* "CHAT")
(defvar *friends-ns* "FRND")
(defvar *followers-ns* "FOLW")
(defvar *mailbox-ns* "MLBX")
(defvar *follower-mailboxes-ns* "FMBX")
(defvar *post-counter* "PST-CNT")

(defvar *expire-days* 1)
(defvar *login-timeout* (* *expire-days* 24 60 60))

(defun uuid-string (&optional (uuid (uuid:make-v4-uuid)))
  (with-open-stream (s (make-string-output-stream))
    (uuid:print-bytes s uuid)
    (get-output-stream-string s)))

(defvar *salt* (let ((salt (redis:with-connection () (redis:red-get "PASSWORD:SALT"))))
		       (if salt 
			   salt
			   (let ((uuid (uuid-string))) 
			     (redis:with-connection () (redis:red-set "PASSWORD:SALT" uuid))
			     uuid))))

(defvar *site-cookie-name* (uuid-string)) ;;can go into redis later on.

(defvar *auth-code* "lisp rocks")

(setf *yaws-server-node-name* "jon-VirtualBox")
(setf *cookie-file* "/home/jon/Dropbox/Lisp-on-Yaws/COOKIE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;really should compare the connection, but i don't plan to use more than 1 connection.

  (defmacro with-transaction (&body body)
    (let ((completed (gensym)))
      `(let ((,completed nil))
	 (unwind-protect
	      (progn 
		(redis:red-multi)
		,@body
		(redis:red-exec)
		(setf ,completed t))
	   (unless ,completed (redis:red-discard))))))

  (ps:defpsmacro js-link (link div-id &optional afterfn object)
    (let ((data (gensym)))
      `($.get ,link
	      ,(if object object `(ps:create))
	      (lambda (,data)
		(console.log ,data)
		(ps:chain ($ ,div-id) 
			  (html ,data))
		,@(if afterfn
		      `((,afterfn))
		      ())))))

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

  (defmacro reply-status (status &rest plist)
    (let (values-list
	  keys-list
	  gsyms)
      (let ((odd t))
	(map nil
	     #'(lambda (val) 
		 (if (stringp val)
		     (if odd
			 (push val keys-list)
			 (push val values-list))
		     (let ((gsym (gensym)))
		       (push (list gsym val) gsyms)
		       (if odd
			   (push gsym keys-list)
			   (push gsym values-list))))
		 (setf odd (not odd))) plist))
      (let ((body
	     `(reply (json:encode-json-to-string (list (cons "status" ,status)
						       ,@(mapcar (lambda (key val) `(cons ,key ,val)) 
								 keys-list
								 values-list))))))
	(if gsyms
	    `(let ,gsyms
	       ,body)
	    body))))
					   
  (defun clickable-li (stream name &rest rest)
    (cl-who:with-html-output (stream)
      (:li :onclick (ps:ps-inline* `(js-link ,@rest))
	   (cl-who:str name)))))

(defun predicate (key ns) (concatenate 'string ns ":" key))

(defun timestamp ()
  (multiple-value-bind (second minute hour date month year)  
      (decode-universal-time (get-universal-time))
    (format nil "~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun read-body (stream &optional (len (file-length stream)))
  (let* ((pos (file-position stream))
	 (body-string (make-string (- len pos))))
    (read-sequence body-string stream)
    body-string))

(defun getprop (list indicator &optional (default nil))
  (do* ((key (car list) (car rest))
	(val (cadr list) (cadr rest))
	(rest (cddr list) (cddr rest)))
       ((or (string-equal key indicator) (null key)) (or val default))))

(defmacro destructure-props (indicator-plist list &body body)
  (let ((gkey (gensym))
	(glist (gensym))
	(gval (gensym))
	(grest (gensym)))
    `(let ((,glist ,list))
       (let (,@(mapcar #'first indicator-plist))
	 (do* ((,gkey (car ,glist) (car ,grest))
	       (,gval (cadr ,glist) (cadr ,grest))
	       (,grest (cddr ,glist) (cddr ,grest)))
	      ((null ,gkey))
	   (cond ,@(mapcar (lambda (pair) 
			     `((string= ,(second pair) ,gkey)
			       (setf ,(first pair) ,gval)))
			   indicator-plist)))
	 ,@body))))

(defun setredis (key ns val &optional secs)
  (let ((predicated (predicate key ns)))
    (if secs
	(redis:with-recursive-connection ()
	  (first (redis:with-pipelining 
		   (redis:red-set predicated val)
		   (redis:red-expire predicated secs))))
	(redis:with-recursive-connection ()
	  (redis:red-set predicated val)))))



(defun getredis (key ns)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
		       (redis:red-get predicated))))

(defun hgetredis (key field ns)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
      (redis:red-hget predicated field))))

(defun hsetredis (key field value ns)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
      (redis:red-hset predicated field value))))

(defun hmgetredis (key ns)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
      (redis:red-hgetall predicated))))

(defun hmsetredis (key ns &rest vals)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
      (apply #'redis:red-hmset predicated vals))))
 

(defun lpushredis (key ns val)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
      (redis:red-lpush predicated val))))

(defun lrangeredis (key ns start end)
  (let ((predicated (predicate key ns)))
    (redis:with-recursive-connection ()
      (redis:red-lrange predicated start end))))

(defsetf getredis (key ns &optional expire) (store)
  (if expire
      `(setredis ,key ,ns ,store ,expire)
      `(setredis ,key ,ns ,store)))

(defun add-post-to-follower-mailboxes (author post-id)
  (redis:with-recursive-connection ()
    (let ((mailboxes (redis:red-smembers (predicate author *follower-mailboxes-ns*))))
      (redis:with-pipelining 
	(dolist (mailbox mailboxes)
	  (redis:red-lpush mailbox post-id))))))

(defun generate-post-html (post-id universal-time author title body)
  (multiple-value-bind (second minute hour date month year)  (decode-universal-time universal-time)
    (declare (ignore second))
    (let* ((body-string (let ((s (make-string-output-stream))) 
			  (cl-markdown:markdown body :stream s)
			  (get-output-stream-string s)))
	   (date-string (format nil "At ~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d" year month date hour minute))
	   (page (cl-who:with-html-output-to-string (var)
		   (:h2 
		    (:a :href 
			(format nil "/blog/viewpost/~a" post-id)
			(cl-who:str title)))
		   (cl-who:str body-string)
		   (:a :href (cl-who:str (format nil "/blog/main/~a" author))
		       (:h4 (cl-who:str (hgetredis author "display-name" *settings-ns*))))
		   (:h4 (cl-who:str date-string)))))
      page)))


(defun generate-post-from-db (pst-id)
  ;(format t "~s~%" 1)
  (let ((pst-props (hmgetredis pst-id *pst-ns*)))
    (if pst-props
	(destructure-props ((universal-time "time")
			    (author "author")
			    (title "title")
			    (body "body"))
	    pst-props
	  (let ((time (parse-integer universal-time)))
	    (generate-post-html pst-id time author title body)))
	"")))

(defun generate-post-entry (title author lines)
  (let* ((time (get-universal-time))
	 (post-id (redis:with-recursive-connection ()
		    (first (fourth (redis:with-pipelining 
				     (with-transaction 
				       (redis:red-get *post-counter*)
				       (redis:red-incr *post-counter*))))))))
    (format t "~s~%" post-id)
    (redis:with-recursive-connection ()
      (redis:with-pipelining
	(lpushredis author *pst-idx* post-id)
	(hmsetredis post-id *pst-ns* 
		    "title" title
		    "author" author
		    "time" time
		    "body" lines))
      (add-post-to-follower-mailboxes author post-id))
    post-id))




(defun most-recent-post (author)
  (first (lrangeredis author *pst-idx* 0 0)))

(defun generate-index (author &key (start 0) (end -1))
  (let ((post-index (lrangeredis author *pst-idx*  start end)))
    (cl-who:with-html-output-to-string (var)
      (:ul :class "navbar"
	   (loop for id in post-index
	      do 
	      (let ((link (format nil "/blog/viewpost/~a" id)))
		(clickable-li var (hgetredis id "title" *pst-ns*)  link "div#blog"))))
      (:input :type "hidden" :id "latest" :name "latest" :value (most-recent-post author)))))

(defun obfuscate-password (password)
  (let* ((salted (concatenate 'string *salt* password)))
    (map 'string #'code-char (md5::MD5SUM-SEQUENCE salted))))

(defun add-password (name password)
    (setf (getredis (string-downcase name) *password-ns*) (obfuscate-password password)))

(defun check-password (name password)
  (string= (getredis (string-downcase name) *password-ns*) (obfuscate-password password)))


 
(defun named-link (stream name &rest rest)
  (cl-who:with-html-output (stream)
    (:a :href "#" :onclick  
	(ps:ps-inline* `(js-link ,@rest))
	(cl-who:str name))))

;(init-appmod blog)

(defhandler (blog get ("viewpost" postid)) (:|html|)
  (if postid
      (reply (generate-post-from-db postid))
      (reply "")))

(defhandler (blog get ("index" author)) (:|html|)
  (reply (generate-index author)))
  

(defhandler (blog get ("last_post" author)) (:|content| "application/json")
    (reply (most-recent-post author)))

(defhandler (blog get ("jslib" author)) (:|content| "application/javascript")
  (setf author (string-downcase author))
  (reply 
   (concatenate 'string  
		(let ((link (format nil "/blog/viewpost/~a" (most-recent-post author))))
		  (ps:ps* 
		   `(defvar author ,author)
		   `(defun get-init-post ()
		      (js-link ,link "div#blog"))
		   `(defun init-login ()
		      (let ((session-id (get-cookie ,*site-cookie-name*)))
			($.post "/blog/re-auth/" (ps:create :session-id session-id)
				(lambda (data textstatus qxhr)
				  (if (equal (ps:getprop data 'status) "success")
				      (progn
					(ps:chain ($ "input#session-id") (val session-id))
					(after-login (ps:getprop data 'user))
					(js-link "/blog/chat/" "div#chat" chat-loop-init))
				      (js-link "/blog/login/" "div#login"))))))
		   `(defun log-out ()
		      (set-cookie ,*site-cookie-name* "" 0)
		      (ps:chain ($ "div#chat") (html ""))
		      (init-login))

		   `(defun update-index ()
		      ($.get ,(format nil "/blog/index/~a" author)
			     (ps:create)
			     (lambda (data)
			       (ps:chain 
				($ "div#index")
				(html data)))))

		   `(defun check-last-post ()
		      ($.get ,(format nil "/blog/last_post/~a" author)  
			     (ps:create)
			     (lambda (server-id)
			       (let ((this-id (ps:chain ($ "input#latest") (val))))
				 (unless (equal this-id server-id)
				   (update-index)
				   (ps:chain ($ "input#latest") (val server-id)))))
			     "json"))))
		(ps:ps
		  (defun after-login (user)
		    (let ((a (concatenate 'string  "Logged In As " user))
			  (b (ps:lisp (cl-who:with-html-output-to-string (s) 
					:br
					(:a :href "#" :onclick (ps:ps-inline (log-out)) "Log Out")))))
		      (ps:chain ($ "div#login") (html (concatenate 'string a b)))
		      (when (equal user author)
			(ps:chain 
			 ($ "div#menu") 
			 (html 
			  (ps:lisp 
			   (cl-who:with-html-output-to-string (var)
			     (:ul
			      (clickable-li var "Add A Post" "/blog/post/new" "div#blog")
			      (clickable-li var "Edit a Post" "/blog/post/edit" "div#blog")
			      (clickable-li var "View Feed" "/blog/friend-feed/" "div#blog"
					    '(lambda ())
					    '(ps:create 
					      :session-id (ps:chain ($ "input#session-id") (val))
					      :start 0
					      :end 20))
			      (clickable-li var
					    "Settings"
					    "/blog/settings"
					    "div#blog" 
					    '(lambda ())
					    '(ps:create :session-id (ps:chain ($ "input#session-id") (val)))))))))
		        )))

		  (defun set-cookie (c-name value exdays)
		    (let ((exdate (ps:new (-date))))
		      (ps:chain exdate (set-date (+ (ps:chain exdate (get-Date)) exdays)))
		      (let ((c-val (concatenate 
				    'string
				    (escape value)
				    (if (not exdays)
					""
					(concatenate 
					 'string
					 #.(format nil ";~%expires=")
					 (ps:chain exdate (to-u-t-c-string)))))))
			(setf (ps:chain document cookie)
			      (concatenate 'string c-name "=" c-val)))))

		  (defun get-cookie (cname)
		    (let ((arr-cookies  (ps:chain document cookie (split ";"))))
		      (let (eqlidx x y r) 
			(do* ((i 0 (+ i 1))
			      (current (ps:getprop arr-cookies i) (ps:getprop arr-cookies i)))
			     ((or (equal r cname) (>= i (ps:chain arr-cookies length)))
			      (if (equal r cname)  y  y))
			  (setf eqlidx (ps:chain current (index-of "=")))
			  (setf x (ps:chain current (substr 0 eqlidx)))
			  (setf y  (ps:chain current (substr (+ eqlidx 1))))
			  (setf r (ps:chain x (replace (ps:regex "/^\s|\s|$/g") "")))))))
				
		  (defun poll-index ()
		    (ps:var timer (set-interval "checkLastPost()" 30000)))

		  (defpostfn make-post (blog post new)
		    ((session-id title text)
		     (ps:create "session-id" session-id
				"title" title
				"post" text))
		    ((data textstatus qxhr)
		     (let ((status (ps:getprop data 'status)))
		       (if (equal status "success")
			   (let* ((most-recent-post (ps:getprop data 'post-id))
				  (user (ps:getprop data 'user))
				  (posts-link (concatenate 'string
							   "/blog/viewpost/" most-recent-post))
				  (indexes-link (concatenate 'string "/blog/index/" user)))
			     (js-link posts-link "div#blog")
			     (js-link "/blog/chat/" "div#chat" chat-loop-init)
			     (js-link indexes-link "div#index")
			     (ps:chain ($ "div#notify") (html (concatenate 'string "Post Success!"))))

			   (ps:chain ($ "div#notify") 
				     (html (concatenate 'string "Post Failure!")))))))

		  (defpostfn update-settings (blog settings)
		    ((object) object)
		    ((data texstatus qxhr)
		     (let ((status (ps:getprop data 'status)))
		       (if (eql status "success")
			   (ps:chain ($ "div#notify") (html "Settings Updated"))
			   (ps:chain ($ "div#notify") (html "Settings Not Upated"))))))

		  (defpostfn login (blog login)
		    ((user password) 
		     (ps:create "user" user "password" password))
		    ((data texstatus qxhr)
		     (let ((status (ps:getprop data 'status)))
		       (when (eql status "success")
			 (let ((expires (ps:getprop data 'expires))
			       (cookie-id (ps:getprop data 'cookie-id))
			       (session-id (ps:getprop data 'session-id)))
			   (ps:chain ($ "input#session-id") (val session-id))
			   (set-cookie cookie-id session-id expires)
			   (after-login user)
			   (js-link "/blog/chat/" "div#chat" chat-loop-init))))))

		  (defpostfn add-friend (blog friends json add)
		    (()
		     (ps:create "session-id" (ps:chain ($ "input#session-id") (val))
				"friends" author))
		    ((data texstatus qxhr)))

		  (defun chat-loop-init ()
		    ($.get (concatenate 'string "/blog/chat/instant/" author)
			   (ps:create :session-id 
				      (ps:chain ($ "input#session-id")
						(val)))
			   (lambda (data)
			     (ps:chain ($ "div#chatwindow")
				       (html data))
			     (chat-loop))))
		  (defun chat-loop ()
		    ($.get (concatenate 'string "/blog/chat/wait/" author)
			   (ps:create :session-id 
				      (ps:chain ($ "input#session-id")
						(val)))
			   (lambda (data)
			     (ps:chain ($ "div#chatwindow") 
				       (html data))
			     (chat-loop))))
			       
		  (defun chat-history (start end)
		    ($.get (concatenate 'string "/blog/chat/history/" author)
			   (ps:create :session-id (ps:chain ($ "input#session-id") (val))
				      :start start
				      :end end)
			   (lambda (data)
			     (let ((status (ps:getprop data 'status)))
			       (if (eql status "success")
				   (let ((result (ps:getprop data 'result)))
				     (ps:chain ($ "div#blog")
					       (html result))))))))

		  (defun key-stroke-update (event)
		    (if (or (= (ps:chain event char-code) 13)
			    (= (ps:chain event key-code) 13))
			(post-chat-msg)))

		  (defun post-chat-msg () 
		    ($.post
		     (concatenate 'string "/blog/chat/p/" author)
		     (ps:create 
		      :message 
		      (ps:chain 
		       ($ "input#message")
		       (val))
		      :session-id
		      (ps:chain 
		       ($ "input#session-id")
		       (val))))
		    (ps:chain ($ "input#message") (val "")))))))

#||#

(defhandler (blog get ("main" author)) (:|html|)
  (setf author (string-downcase author))
  (let ((properties (hmgetredis author *settings-ns*)))
    (reply 
     (cl-who:with-html-output-to-string (var)
       (:html (:head (:title (cl-who:str (getprop properties "title")))
		     (:link :rel "stylesheet" :href "/blog.css"))
	      (:body 
	       (:div :id "header" :class "header"
		     (:div  :id "notify" :class "notify")
		     (:div :id "login" :class "login")
		     (:h1 (cl-who:str (getprop properties "title")))
		     (:h4 (cl-who:str (getprop properties "subtitle"))))

	       (:div :id "index" :class "index")
	       (:div :id "chat" :class "chat")
	       (:div  :id "blog" :class "blog")	     
	  
	       (:div  :id "menu" :class "menu"
		      (:ul
		       
		       (clickable-li var "Friends" "/blog/friends" "div#blog"
				     '(lambda())
				     '(ps:create
				       :session-id (ps:chain ($ "input#session-id") (val))
				       :user author))
		       (cl-who:htm (:li :onclick (ps:ps-inline
						  (add-friend))
					"Add as Friend"))))

	       (:script :src "/jquery.min.js")
	       (:script :src (format nil "/blog/jslib/~a" author))

	       (cl-who:htm
		(:script :type "text/javascript"
			 (cl-who:str
			  (ps:ps 
			    (ps:chain 
			     ($ document) 
			     (ready
			      (lambda ()
				(init-login)
				(get-init-post)		  
				(update-index)
				(poll-index)))))))
		(:input :type "hidden" :id "session-id" :name "session-id"))))))))

(defhandler (blog get ("post" "new")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:body
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

(defhandler (blog post ("post" "new")) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (title (second (assoc "title" q :test #'string=)))
	 (post (second (assoc "post" q :test #'string=))))
    (let ((user (check-login session-id)))
      (if (and user title post)	
	  (let ((pst-id (generate-post-entry title user post)))
	    (reply-status "success" 
			  "user" user
			  "postId" pst-id))
	  (reply-status "failure" "user" "" "postId" "")))))

(defhandler (blog get ("post" "edit")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var))))

(defhandler (blog post ("post" "edit")) (:|content| "application/json"))

(defhandler (blog get ("friend-feed")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (start (parse-integer (second (assoc "start" q :test #'string=)) :junk-allowed t))
	 (end (parse-integer (second (assoc "end" q :test #'string=)) :junk-allowed t)))
    (let ((user (check-login session-id)))
      (if (and user start end)
	  (redis:with-recursive-connection ()
	    (let* ((paths (lrangeredis user *mailbox-ns* start end))
		   (titles (redis:with-pipelining 
			     (dolist (path paths)
			       (redis:red-hmget (concatenate 'string *pst-title* ":" path))))))
	      (reply (cl-who:with-html-output-to-string (var)
		       (when (>= start 20) 
			 (cl-who:htm (named-link var "Newer" "/blog/friend-feed" "div#blog"
						 '(lambda ())
						 `(ps:create
						   :session-id (ps:chain ($ "input#session-id") (val))
						   :start ,(- start 20)
						   :end ,(- end 20)))))
		       (cl-who:htm (named-link var "Older" "/blog/friend-feed" "div#blog"
					       '(lambda ())
					       `(ps:create
						 :session-id (ps:chain ($ "input#session-id") (val))
						 :start ,(+ start 20)
						 :end ,(+ end 20))) :br)
		       (map 'nil 
			    (lambda (path title)
			      (when (and path title)
				(cl-who:htm 
				 (named-link var title (format nil "/blog/viewpost/~a" path) "div#blog") :br))) paths titles)))))
	  (reply "")))))

(defhandler (blog get ("settings")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (let ((user (check-login session-id)))
      (if user
	  (let* ((settings (hmgetredis user *settings-ns*)))
	    (let ((title (getprop settings "title" "Title"))
		  (display-name (getprop settings "display-name" user))
		  (subtitle (getprop settings "subtitle" "Subtitle")))
	      (reply (cl-who:with-html-output-to-string (var)
		       (:body
			"Display Name" :br
			(:input :type "text" :id "display-name" :name "display-name" :value display-name) :br
			"Title" :br
			(:input :type "text" :id "title" :name "title" :value title) :br
			"Subtitle" :br
			(:input :type "text" :id "subtitle" :name "subtitle" :value subtitle) :br
			(:input 
			 :type "submit"
			 :name "submit"
			 :onclick (cl-who:str (ps:ps-inline (update-settings
							     (ps:create 
							      :session-id 
							      (ps:chain ($ "input#session-id")
									(val))
							      :display-name 
							      (ps:chain ($ "input#display-name")
									(val))
							      :title
							      (ps:chain ($ "input#title")
									(val))
							      :subtitle
							      (ps:chain ($ "input#subtitle")
									(val))))))
			 :value "Update Settings"))))))
	  (reply "Log In First"))
      )))

(defhandler (blog post ("settings")) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    ;(format t "~s~%" q)
    (let ((user (check-login session-id)))
      (if user
	  (let ((title (cl-who:escape-string-iso-8859-1(second (assoc "title" q :test #'string=))))
		(subtitle (cl-who:escape-string-iso-8859-1 (second (assoc "subtitle" q :test #'string=))))
		(display-name (cl-who:escape-string-iso-8859-1 (second (assoc "display-name" q :test #'string=)))))
	    (hmsetredis user *settings-ns* "title" title "subtitle" subtitle "display-name" display-name)
	    (reply-status "success"))
	  (reply-status "failure")))))

(defhandler (blog get ("register")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:title "Registration")
		  (:body (:B "Register to Post and Chat")
			 (:form :action "/blog/register" :method "POST"
				"UserId"
				:br
				(:input :type "text" :name "user")
				:br
				"Password"
				:br
				(:input :type "password" :name "password")
				:br
				"Password Again"
				:br
				(:input :type "password" :name "password2")
				:br
				"E-mail"
				:br
				(:input :type "text" :name "e-mail")
				:br
				(:input :type "submit" :value "Register")))))))

(defvar *users* "USERS")

(defhandler (blog post ("register")) (:|content| "application/json")
  (let*  ((q (parse-query *query*))
	  (e-mail (second (assoc "e-mail" q :test #'string=)))
	  (user (string-downcase (cl-who:escape-string-iso-8859-1 (second (assoc "user" q :test #'string=)))))
	  (password (second (assoc "password" q :test #'string=)))
	  (password2 (second (assoc "password2" q :test #'string=))))

    (cond 
      ((or (getredis user *password-ns*)
	   (< (length user) 3))
       (reply (cl-who:with-html-output-to-string (var)
		(:html (:body (:B "Name already taken or name must be at least 3 characters")
			      :br (:b (:a :href "/blog/register" "Try Again")))))))
      ((string= password password2)
       (redis:with-recursive-connection ()
			  (redis:red-sadd *users* user)
			  (add-password user password)
			  (hmsetredis user *settings-ns*
				      "title" "Your Title Here"
				      "subtitle" "Go To Settings to Change Title" "display-name" user))
       (reply (format nil "/blog/main/~a" user) :|redirect|))
      (T (reply (cl-who:with-html-output-to-string (var)
		  (:html (:body (:B "Passwords do not match")
				:br (:b (:a :href "/blog/register" "Try Again"))))))))))

(defhandler (blog get ("login")) (:|html|)
  (reply (cl-who:with-html-output-to-string (stream)
	   (:body 
	    "Login Name"
	    :br
	    (:input :type "text" :id "user" :name "user") :br
	    "Password"
	    :br
	    (:input :type "password" :id "password" :name "password") :br
	    (:input :type "submit" :value "Login" :onclick 
		    (ps:ps-inline (login
				   (ps:chain ($ "input#user")
					     (val))
				   (ps:chain ($ "input#password")
					     (val)))))
	    :br 
	    "Don't Have an Account? " 
	    (named-link stream "Register" "/blog/register/" "div#blog" )
	    ))))




(defun create-login (user password)
  (when (check-password user password)
    (let* ((uuid (uuid-string)))
      (setf (getredis uuid *login-cookie-ns* *login-timeout*) (string-downcase user))
      uuid)))

(defun check-login (uuid)
  (let ((user (getredis uuid *login-cookie-ns*)))
    (when user
      (setf (getredis uuid *login-cookie-ns* *login-timeout*) user) user)))

(defhandler (blog post ("login")) (:|content| "application/json")
  (let ((q (parse-query *query*)))
    (let ((user (string-downcase (cl-who:escape-string-iso-8859-1 (second (assoc "user" q  :test #'string=)))))
	  (password (second (assoc "password" q :test #'string=))))
      (let ((uuid (create-login user password)))
	(if uuid 
	    (reply-status "success" 
			  "expires" *expire-days*
			  "cookieId" *site-cookie-name*
			  "postId" (most-recent-post user)
			  "sessionId" uuid)
	    (reply-status "failure"))))))

(defhandler (blog post ("re-auth")) (:|content| "application/json")
  (let ((q (parse-query *query*)))
    (let ((session-id (second (assoc "session-id" q :test #'string=))))
      (let ((user (check-login session-id)))
	(if user
	    (reply-status "success" "user" user)
	    (reply-status "failure" "user" ""))))))



(let ((chat-reply-table (make-hash-table :test  #'equalp :synchronized t)))

  (defun get-chat-text (user &optional (chat-length 20))
    (let ((text-list (lrangeredis user *chat-ns* 0 chat-length)))
      (apply #'concatenate 'string (nreverse text-list))))

  (defun set-chat-text (user string)
    (lpushredis user *chat-ns* string)
    (reply-chat user))

  (defun queue-request (user) 
    (sb-ext:with-locked-hash-table (chat-reply-table)
      (push (get-reply-information) (gethash user chat-reply-table nil))))

  (defun reply-chat (user)
    (let ((chat-reply-list (sb-ext:with-locked-hash-table (chat-reply-table)
			     (prog1 (gethash user chat-reply-table)
			       (setf (gethash user chat-reply-table) nil)))))
      (let ((text (get-chat-text user)))
	(reply-all text  chat-reply-list :|html|)))))

(defhandler (blog get ("chat")) (:|html|)
  (reply (cl-who:with-html-output-to-string (val)
	   (:html (:body 
		   (:a :href "#" :onclick (ps:ps-inline (chat-history 0 20)) "history")
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
			   :onclick (ps:ps-inline (post-chat-msg))))))))

(defhandler (blog get ("chat" "wait" author)) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (when (check-login session-id)
      (queue-request author))))

(defhandler (blog get ("chat" "instant" author)) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (when (check-login session-id)
      (reply (get-chat-text author)))))

(defhandler (blog get ("chat" "history" author)) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (start (parse-integer (second (assoc "start" q :test #'string=)) :junk-allowed t))
	 (end (parse-integer (second (assoc "end" q :test #'string=)) :junk-allowed t)))
    (if (and (check-login session-id) start end)
	(reply-status 
	 "success"
	 "result"
	 (cl-who:with-html-output-to-string (var)
	   (when (>= start 20) 
	     (cl-who:htm (:a :href "#" :onclick (ps:ps-inline* `(chat-history 
								 ,(- start 20)
								 ,(- end 20))) "Newer")
			 (cl-who:str " ")))
	   (cl-who:htm (:a :href "#" :onclick (ps:ps-inline* `(chat-history
							       ,(+ start 20)
							       ,(+ end 20))) "Older") :br)

	   (cl-who:str (apply #'concatenate 'string (nreverse (lrangeredis author *chat-ns* start end))))))
	(reply-status "failure"))))
			      

(defhandler (blog post ("chat" "p" author)) (:|html|)
  (reply "")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (message (second (assoc "message" q :test #'string=))))
    (let ((name (check-login session-id)))
      (when (and name message)
	(set-chat-text author (cl-who:with-html-output-to-string (str)
				(cl-who:str (timestamp)) " " 
				(:a :href (format nil "/blog/main/~a" name) 
				    (cl-who:str (format nil "~a:" (hgetredis name "display-name" *settings-ns*))))
				" "
				(cl-who:str (cl-who:escape-string-iso-8859-1  message))
				:br))))))

(defun friend-p (user friend)
  (let ((predicated (predicate user *friends-ns*)))
    (redis:with-recursive-connection () (redis:red-sismember predicated friend))))

(defun add-friend (user friend)
  (let ((user-friends (predicate user *friends-ns*))
	(friend-followers (predicate friend *followers-ns*))
	(friend-follower-mailboxes (predicate friend *follower-mailboxes-ns*)))
    (when (redis:with-recursive-connection () 
	    (and (redis:red-sismember *users* friend)
		 (redis:red-sismember *users* user)))
      (redis:with-recursive-connection ()
	(redis:with-pipelining 
	  (redis:red-sadd user-friends friend)
	  (redis:red-sadd friend-followers user)
	  (redis:red-sadd friend-follower-mailboxes (predicate user *mailbox-ns*)))))))

(defun get-friends (user)
  (let ((predicated (predicate user *friends-ns*)))
    (redis:with-recursive-connection ()
      (redis:red-smembers predicated))))

(defhandler (blog post ("friends" "json" "add")) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (user (check-login session-id)))
    (block nil
      (when user
	(let ((friends (second (assoc "friends" q :test #'string=))))
	  (when friends
	    (let ((friends-list (cl-ppcre:split " " friends)))
	      (dolist (friend friends-list)
		(add-friend user friend)))
	    (reply-status "success")
	    (return))))
      (reply-status "failure"))))

(defhandler (blog get ("friends")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (user (check-login session-id)))
    (block nil
      (when user
	(let ((user-to-lookup (second (assoc "user" q :test #'string=))))
	  (let ((friends-list (when user-to-lookup
				(get-friends user-to-lookup))))
	    (when friends-list
	      (reply 
	       (cl-who:with-html-output-to-string (var)
		 (dolist (friend friends-list)
		   (cl-who:htm (:a :href (format nil "/blog/main/~a" friend) (cl-who:str friend)) :br))))
	      (return)))))
      (reply ""))))

(defhandler (blog get ("friends" "json" "list")) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (user (check-login session-id)))
    (block nil
      (when user
	(let ((user-to-lookup (second (assoc "user" q :test #'string=))))
	  (let ((friends-list (when user-to-lookup
				(get-friends user-to-lookup))))
	    (when friends-list
	      (reply-status "success" "friends" friends-list)
	      (return))))))
    (reply-status "failure")))
	
(generate-appmods)

(defun blog-main ()
  (redis:with-recursive-connection ()
    (unless (redis:red-get *post-counter*) (redis:red-set *post-counter* 0)))
  (init-server-connection)
  (generate-appmods))