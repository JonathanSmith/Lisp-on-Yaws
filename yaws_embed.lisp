(defparameter *yaws-server-node* (cleric-epmd:lookup-node "yaws" "jon-VirtualBox"))
(defvar *yaws-listener-thread*)
(defvar *pids-hash* (make-hash-table :test 'equalp))
(defvar *reg-pids-hash* (make-hash-table :test 'equalp))
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
  (let* ((to-symbol (cleric:to-name control-message))
	 (to-name (symbol-name to-symbol))
	 (fnlambda (gethash to-name *reg-pids-hash*)))
    (when fnlambda
      (funcall fnlambda control-message))))

(defmethod hash-dispatch ((control-message cleric:send))
  (let* ((to-pid (cleric:to-pid control-message))
	 (pid-id (pid-id to-pid))
	 (fnlambda (gethash pid-id *pids-hash*)))
    (when fnlambda (funcall fnlambda control-message))))

(defun pid-id (pid)
  (slot-value pid 'cleric::id))

(defun init ()
  (cleric:remote-node-connect *yaws-server-node* "foo")
  (node-listener-thread #'hash-dispatch)
  (let ((pid (set-hash-dispatch #'printmsg)))
    (register-name #'printmsg "printmsg")
    ;(cleric:reg-send *pid* "cleric_listener" "yaws" pid)
    ))

(defun register-name (fn name)
  (setf (gethash name *reg-pids-hash*) fn))

(defun set-hash-dispatch (fn &optional (pid (cleric:make-pid)))
  (let* ((pid-id (pid-id pid)))
    (setf (gethash pid-id *pids-hash*) fn)
    pid))

(defun printmsg (control-message)
  (format t "~s~%" (cleric:message control-message)))

;(cleric:reg-send *pid* "cleric_listener" "yaws" "Hello, Erlang world!")