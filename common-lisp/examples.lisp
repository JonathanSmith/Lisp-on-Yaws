(setf *yaws-server-node-name* "jon-VirtualBox")
(setf *cookie-file* "/home/jon/Lisp-On-Yaws/COOKIE")

(defun init ()
  (let ((yaws-server (cleric-epmd:lookup-node "yaws" *yaws-server-node-name*)))
    (let ((cookie (with-open-file (stream *cookie-file* :direction :input)
		    (read-line stream))))
      (cleric:remote-node-connect yaws-server cookie)))
  (node-listener-thread #'hash-dispatch)
  (let ((hello-world-lfe (write-module-string "hello_world" (cleric:this-node) '(hello_world GET ()))))
    (send-file-for-compilation "hello_world" hello-world-lfe))
  (add-appmod (cleric:make-atom "hello_world"))
  (register "hello_world" (easy-handler () (:|html|)
			    (reply "hello world"))))

(defun add-math-appmod ()
  (let ((hello-world-lfe (write-module-string "basic_math" (cleric:this-node) 
					      '(addition GET ("+" B C))
					      '(subtraction GET ("-" B C))
					      '(multiplication GET ("*" B C))
					      '(equals GET ("=" B C)))))
    (send-file-for-compilation "basic_math" hello-world-lfe)
    (add-appmod (cleric:tuple  "math" (cleric:make-atom "basic_math")))
    (macrolet ((parse-or-reply (symbol &optional (emessage "error"))
		 (assert (symbolp symbol))
		 `(handler-case (parse-integer ,symbol)
		    (sb-int::simple-parse-error () 
		      (reply ,emessage)
		      (return)))))
      (register "addition"
		(easy-handler (a b) (:|html|)
		  (block nil
		    (if (and a b) 
			(let* ((a-int (parse-or-reply a "parse error on a"))		  
			       (b-int (parse-or-reply b "parse error on b")))
			  (reply (format nil "~s" (+ a-int b-int))))
			(reply "not enough args")))))
      (register "subtraction"
		(easy-handler (a b) (:|html|)
		  (block nil
		    (if (and a b) 
			(let* ((a-int (parse-or-reply a "parse error on a"))		  
			       (b-int (parse-or-reply b "parse error on b")))
			  (reply (format nil "~s" (- a-int b-int))))
			(reply "not enough args")))))
      (register "multiplication"
		(easy-handler (a b) (:|html|)
		  (block nil
		    (if (and a b) 
			(let* ((a-int (parse-or-reply a "parse error on a"))		  
			       (b-int (parse-or-reply b "parse error on b")))
			  (reply (format nil "~s" (* a-int b-int))))
			(reply "not enough args")))))
      (register "equals"
		(easy-handler (a b) (:|html|)
		  (block nil
		    (if (and a b) 
			(let* ((a-int (parse-or-reply a "parse error on a"))		  
			       (b-int (parse-or-reply b "parse error on b")))
			  (reply (if (= a-int b-int) "true" "false")))
			(reply "not enough args"))))))))