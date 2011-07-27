(defparameter *yaws-server-node* (cleric-epmd:lookup-node "yaws" "jon-VirtualBox"))
(defvar *yaws-listener-thread*)
(defvar *pids-hash* (make-hash-table))
(defvar *reg-pids-hash* (make-hash-table))
(defvar *pid* (cleric:make-pid))

(defun node-listener-thread (dispatch-function)
  (setf *yaws-listener-thread*
	(sb-thread:make-thread 
	 (lambda ()
	   (do ()
	       (NIL)
	     (let ((messages (cleric:receive-node-messages)))
	       (when messages (mapcar dispatch-function messages))))))))

(defmethod hash-dispatch ((control-message cleric:reg-send))
  (let* ((to-name (cleric:to-name control-message))
	(fnlambda (gethash to-name *reg-pids-hash*)))
    (funcall (funcall fnlambda control-message))))

(defmethod hash-dispatch ((control-message cleric:send))
  (let* ((to-pid (cleric:to-pid control-message))
	 (fnlambda (gethash to-pid *pids-hash*)))
    (when fnlambda (funcall fnlambda control-message))))

(defun set-hash-dispatch (fn)
  (let ((pid (cleric:make-pid)))
    (setf (gethash pid *pids-hash*) fn)
    pid))

(defun register (string fn)
  (setf (gethash string *reg-pids-hash* ) fn)
  (setf (gethash (cleric:string-to-binary string) *reg-pids-hash* ) fn))
  

(defvar +file-atom+ (cleric:make-atom "file"))

(defun write-module-string (module-name node-name &rest resources)
  (let ((header (format nil "(defmodule ~a (export (out 1)))" module-name))
	(include (format nil "(include-file \"yaws_api.lfe\")" ))
	(gen-out (format nil "(gen_resources ~a (~{~s~^ ~}))" node-name resources)))
    (format nil "~a~%~a~%~a~%" header include gen-out)))

(defun send-file-for-compilation (module-name file-data)
  (cleric:reg-send *pid* "lfe_compiler" "yaws" 
		   (cleric:tuple +file-atom+  module-name file-data)))

(defvar +update_appmod+ (cleric:make-atom "update_appmod"))

(defun add-appmod (module-name)
  (cleric:reg-send *pid* "conf_control" "yaws"
		   (cleric:tuple +update_appmod+ (cleric:make-atom module-name))))

(defun hello-world (control-message)
  (let* ((msg (cleric:message control-message))
	(vector (cleric:elements msg)))
    (format t "~a,~a,~a~%" control-message msg vector)
    (let ((type (elt vector 0))
	  (dest (elt vector 1))
	  (ref (elt vector 2))
	  (query (elt vector 3)))
      (cleric:send dest (cleric:tuple ref "Hello World")))))

(defun init ()
  (cleric:remote-node-connect *yaws-server-node* "foo")
  (node-listener-thread #'hash-dispatch)
  (send-file-for-compilation "hello_world" (write-module-string "hello_world" (cleric:this-node)))
  (add-appmod "hello_world")
  (register "hello_world" #'hello-world))





;(cleric:reg-send *pid* "cleric_listener" "yaws" "Hello, Erlang world!")