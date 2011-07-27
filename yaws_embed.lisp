(defparameter *yaws-server-node* (cleric-epmd:lookup-node "yaws" "jon-desktop"))
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

(defun init ()
  (cleric:remote-node-connect *yaws-server-node* "foo")
  ;(cleric:reg-send *pid* "cleric_listener" "yaws" "Hello, Erlang")
  (node-listener-thread #'hash-dispatch)
)

(defun set-hash-dispatch (fn)
  (let ((pid (cleric:make-pid)))
    (setf (gethash pid *pids-hash*) fn)
    pid))

(defvar +file-atom+ (cleric:make-atom "file"))

(defun write-module-string (module-name node-name &rest resources)
  (let ((header (format nil "(defmodule ~a (export (out 1)))" module-name))
	(include (list 'include-file "yaws_api.lfe"))
	(gen-out (format nil "(gen_resources ~a (~{~s~^ ~}))" node-name resources)))
    (format nil "~s~%~s~%~s~%" header include gen-out)))

(defun send-file-for-compilation (module-name file-data)
  (cleric:reg-send *pid* "lfe_compiler" "yaws" 
		   (cleric:tuple +file-atom+  module-name file-data)))

(defvar +update_appmod+ (cleric:make-atom "update_appmod"))

(defun add-appmod (module-name)
  (cleric:reg-send *pid* "lfe_compiler" "yaws"
		   (cleric:tuple +update_appmod+ (cleric:make-atom module-name))))



;(cleric:reg-send *pid* "cleric_listener" "yaws" "Hello, Erlang world!")