(defparameter *yaws-server-node* (cleric-epmd:lookup-node "yaws" "jon-VirtualBox"))
(defvar *yaws-listener-thread*)
(defvar *pids-hash* (make-hash-table))
(defvar *reg-pids-hash* (make-hash-table))
(defvar *pid* (cleric:make-pid))
;

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
  (cleric:reg-send *pid* "cleric_listener" "yaws" "Hello, Erlang")
  ;(node-listener-thread #'hash-dispatch)
)

(defun set-hash-dispatch (fn)
  (let ((pid (cleric:make-pid)))
    (setf (gethash pid *pids-hash*) fn)
    pid))

(defun printmsg (control-message)
  (format t "~s~%" control-message))

;(cleric:reg-send *pid* "cleric_listener" "yaws" "Hello, Erlang world!")