;%%%----------------------------------------------------------------------
;%%% File    : yaws_api.hrl
;%%% Author  : Claes Wikstrom <klacke@hyber.org>
;%%% Purpose : 
;%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
;%%%----------------------------------------------------------------------
;(defmodule yaws_api (export (out 1)))
;updated by Jonathan Smith 11/23/2009 for use with LFE

(defrecord arg
          clisock        ;%% the socket leading to the peer client
          client_ip_port ;%% {ClientIp, ClientPort} tuple
          headers        ;%% headers
          req            ;%% request
          clidata        ;%% The client data (as a binary in POST requests)
          server_path    ;%% The normalized server path 
                         ;%% (pre-querystring part of URI)
          querydata      ;%% For URIs of the form ...?querydata 
                         ;%%  equiv of cgi QUERY_STRING
          appmoddata     ;%% (deprecated - use pathinfo instead) the remainder 
			 ;%% of the path leading up to the query
          docroot        ;%% Physical base location of data for this request
          docroot_mount  ;%% virtual directory e.g /myapp/ that the docroot
                         ;%%  refers to.
          fullpath       ;%% full deep path to yaws file
          con            ;%% Continuation for chunked multipart uploads
          state          ;%% State for use by users of the out/1 callback
          pid            ;%% pid of the yaws worker process
          opaque         ;%% useful to pass static data
          appmod_prepath ;%% (deprecated - use prepath instead) path in front 
                         ;%%of: <appmod><appmoddata>
          prepath,       ;%% Path prior to 'dynamic' segment of URI. 
                         ;%%  ie http://some.host/<prepath>/<script-point>/d/e 
                         ;%% where <script-point> is an appmod mount point, 
                         ;%% or .yaws,.php,.cgi,.fcgi etc script file.
          pathinfo       ;%% Set to '/d/e' when calling c.yaws for the request 
                         ;%% http://some.host/a/b/c.yaws/d/e
                         ;%%  equiv of cgi PATH_INFO
         )


(defrecord http_request method path version)

(defrecord http_response version status phrase)

(defrecord headers
  connection
  accept
  host
  if_modified_since
  if_match
  if_none_match
  if_range
  if_unmodified_since
  range
  referer
  user_agent
  accept_ranges
  (cookie '())
  keep_alive
  location
  content_length
  content_type
  content_encoding
  authorization
  transfer_encoding
  (other '())				;%% misc other headers
  )




(defrecord url
  scheme				;%% undefined means not set
  host					;%% undefined means not set
  port					;%% undefined means not set
  (path '())
  (querypart '()))


(defrecord setcookie
  key           
  value         
  quoted        
  comment
  comment_url
  discard
  domain
  max_age
  expires
  path
  port
  secure
  version)


(defrecord redir_self 
          host        ;%% string() - our own host
          scheme      ;%% http | https
          scheme_str  ;%% "https://"  | "http://"
          port        ;%% integer()  - our own port
          port_str     ;%% "" | ":<int>" - the optional port part
                       ;%%                 to append to the url
         )

(defrecord upload
  filename
  partname
  last
  rlist
  data)

;(defrecord node
;    node)

;(defun create-resource (resource)
;  (: mnesia create_table resource '(#(attributes (node)))))

;(defun get-registered-node (resource)
;  (case (: mnesia read resource 'node)
;    ('abort nil)
;    (node-list 
;     (: lists nth 
;	(- (: random uniform (length node-list)) 1) node-list))))

;(defun add-registered-node (resource node)
 ; (: mnesia write resource (make-node node node)))

(eval-when-compile
  (defun make-resource-bodies (X node)
    (let* (([resource method path] X))
      (case path 
	('*? `([arg ',method fullpath]
		    (let* ((to (tuple ',resource ',node))
			   (ref (make_ref))
			   (message ,(case method
					   ('GET `(tuple ',method (self) ref (: yaws_api parse_query arg) fullpath))
					   ('POST `(tuple ',method (self) ref (: yaws_api parse_post arg) fullpath)))))
		      (: erlang send to message)
		      (receive-loop ref))))
	(path
	 (let* ((path* (: lists map (lambda (Y) (if (is_list Y) `(quote ,Y) Y)) path))
		(filtered-path (: lists filter (lambda (Y) (is_atom Y)) path)))
	   (case method
	     ('POST-MULTIPART 
	      `([arg 'POST [_ ,@path*]]
		     (flet* ((return-message (result)
					     ;(: io format '"a~n" (list))
					     (let* ((to (tuple ',resource ',node))
						    (ref (make_ref))
						    (message ,`(tuple 'POST (self) ref result ,@filtered-path)))
					       (: erlang send to message)
					       (receive-loop ref)))

			     (multipart (A State)
					;(: io format '"b~n" (list))
					(let ((Parse (: yaws_api parse_multipart_post A)))
					  ;(: io format '"!!!~p~n" (list Parse))
					  (case Parse
					    (()
					     (return-message '""))
					    ((tuple 'cont Cont Part)
					     (case (process-part A Part State)
					       ((tuple 'done result)
						(return-message result))
					       ((tuple 'cont NewState)
						(tuple 'get_more Cont NewState))))
					    ((tuple 'result Part)
					     ;(: io format '"~p ~p~n" (list 'result Part))
					     (let ((processed (process-part A Part (set-upload-last State 'true))))
					       (case processed
						 ((tuple 'done result)
						  (return-message result))
						 ((tuple 'cont _)
						  (return-message '""))))))))
					
			     (handle-multipart (A)
					       ;(: io format '"c~n"(list))
					       (if (is_record (arg-state A) 'upload)
						   (multipart A (arg-state A))
						   (let ((state (make-upload)))
						     (multipart A state)))))
			    
			    (handle-multipart arg))))
	     (anything
	      `([arg ',method [_ ,@path*]]
		     (let* ((to (tuple ',resource ',node))
			    (ref (make_ref))
			    (content ,(case method 
					    ('GET `(: yaws_api parse_query arg))
					    ('POST `(: yaws_api parse_post arg))))
			    (message ,(case method
					    ('GET `(tuple ',method (self) ref  content ,@filtered-path))
					    ('POST `(tuple ',method (self) ref content ,@filtered-path)))))
		       (: erlang send to message)
		       (receive-loop ref)))))))))))


(defun sanitize-filename (Input)
  (let* ((filepath (case Input
		     (List (when (is_list List)) List)
		     (_ '"unnamed")))
	 ;(strip-windows-dirs-fun (lambda (L) (: lists last (: string tokens L '"\\"))))
	 (strip-unix-dirs-fun (lambda (L) (: lists last (: string tokens L '"/"))))
	 (empty-to-unnamed-fun (lambda (L) (case L
					     (() '"unnamed")
					     (_ L )))))
    (: yaws_api htmlize
       (funcall empty-to-unnamed-fun
		(funcall strip-unix-dirs-fun
			 ;(funcall strip-windows-dirs-fun 
			 filepath)
			 )))););)

(defun process-part  
    ([A (cons (tuple 'part_body Data) Tail) State]
     (: io format '"1~n"(list))
     (process-part A (cons (tuple 'body Data) Tail) State))

  ([_ () State] (when (and (== (upload-last State) 'true) 
			   (/= (upload-filename State) 'undefined)))
      (: io format '"2a~n"(list))
      (let ((data (iolist_to_binary (: lists flatten (: lists reverse (upload-rlist State))))))
	(tuple 'done  (: lists reverse (cons (tuple (upload-partname State) (tuple (upload-filename State) data)) (upload-data State))))))
  
  ([_ () State] (when (and (== (upload-last State) 'true) 
			   (== (upload-filename State) 'undefined)))
      (: io format '"2b~n"(list))
      (let ((data (: lists reverse (upload-rlist State))))
	(tuple 'done  (: lists reverse (cons (tuple (upload-partname State) data) (upload-data State))))))

  ([_ () State] (when (== (upload-last State) 'true))
      (: io format '"3~n"(list))
      (tuple 'done (tuple 'error '"Error: did not receive header with upload.")))

  ([_ () State]
      (: io format '"4~n"(list))
      (tuple 'cont State))

  ([A (cons (tuple 'head (tuple head-name Opts)) Tail) State]
      (: io format '"5~p~n"(list head-name))
      (flet ((add-list (State filename head-name current-data)
	       (process-part A Tail 
			     (set-upload-rlist
			      (set-upload-partname
			       (set-upload-filename
				(set-upload-data
				 State
				 (cons 
				  (tuple 
				   (upload-partname State)
				   (: lists flatten (: lists reverse (upload-rlist State))))
				  current-data)) filename) head-name) (list))))
	     (add-binary (State filename head-name current-data)
	       (process-part A Tail 
			     (set-upload-rlist
			      (set-upload-partname 
			       (set-upload-filename 
				(set-upload-data 
				 State
				 (cons (tuple 
					(upload-partname State) 
					(tuple 
					 (upload-filename State)
					 (iolist_to_binary (: lists reverse (upload-rlist State)))))
				       (upload-data State))) filename) head-name) (list))))
	     (ignore-undefined (State filename)
	       (process-part A Tail 
			     (set-upload-rlist
			      (set-upload-partname
			       (set-upload-filename
				State filename) head-name) (list)))))

	(case (: lists keysearch 'filename 1 Opts)
	  ((tuple 'value (tuple _ UncheckedFileName))
	   (let ((filename (sanitize-filename UncheckedFileName)))
	     (: io format '"7a ~p ~p~n" (list (upload-rlist State) (upload-partname State)))
	     (if (/= (upload-data State) 'undefined)
		 (if (== 'undefined (upload-filename State))
		     (if (== 'undefined (upload-rlist State))
			 (ignore-undefined State filename)
			 (add-list State filename head-name (upload-data State)))
		     (add-binary State filename head-name (upload-data State)))
		 (if (== 'undefined (upload-filename State))
		     (if (== 'undefined (upload-rlist State))
			 (ignore-undefined State filename)
			 (add-list State filename head-name (list)))
		     (add-binary State filename head-name (list))))))
	  ('false
	   (: io format '"7b ~p ~p~n" (list (upload-rlist State) (upload-partname State)))
	   (if (/= (upload-data State) 'undefined)
	       (if (== 'undefined (upload-filename State)) 
		   (if (== 'undefined (upload-rlist State))
		       (ignore-undefined State 'undefined)
		       (add-list State 'undefined head-name (upload-data State)))
		   (add-binary State 'undefined head-name (upload-data State)))

	       (if (== 'undefined (upload-filename State))
		   (if (== 'undefined (upload-rlist State))
		       (ignore-undefined State 'undefined)
		       (add-list State 'undefined head-name (list)))
		   (add-binary State 'undefined head-name (list))))))))

  ([A (cons (tuple 'body Data) Tail) State] (when (/= (upload-filename State) 'undefined))
      (: io format '"6a~n"(list))
      (let ((new-r-list (cons (list_to_binary Data) (upload-rlist State))))
	(process-part A Tail (set-upload-rlist State new-r-list))))

  ([A (cons (tuple 'body Data) Tail) State]
      (: io format '"6b~n"(list))
      (let ((new-r-list (cons Data (upload-rlist State))))
	(process-part A Tail (set-upload-rlist State new-r-list))))

  ([A B C]
      (: io format '"error? ~p ~p ~p~n" (list A B C))))

(defun receive-loop (ref)
  (receive ((tuple ref pid 'ping)
	    (: erlang send pid 'pong)
	    (receive-loop ref))
	   ((tuple ref msg)
	    msg)))

(defun out (arg)
  (let* ((url (: yaws_api request_url arg))
	 (path (: string tokens (url-path url) '"/")))
    ;(: io format '"???~p ~p ~p ~n" (list arg (http_request-method (arg-req arg)) path))
    (out arg (http_request-method (arg-req arg)) path)))

(defmacro gen_resources [node resources]
	  (let ((resource_bodies (: lists map (lambda [X] (make-resource-bodies X node)) resources)))
	    (let ((result `(defun out ,@resource_bodies
			     ([arg method path]
				   (tuple 'status 404)))))

	      ;(: io format '"!!! ~p ~n" (list result))
	      result)))
	      