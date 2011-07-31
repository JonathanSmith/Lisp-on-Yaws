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

(eval-when-compile
  (defun make-resource-bodies (X node)
    (let* (([resource method path] X))
      (: io format '"~p~n" (list X))
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
	   `([arg ',method [_ ,@path*]]
	     (let* ((to (tuple ',resource ',node))
		    (ref (make_ref))
		    (message ,(case method
				('GET `(tuple ',method (self) ref  (: yaws_api parse_query arg) ,@filtered-path))
				('POST `(tuple ',method (self) ref (: yaws_api parse_post arg) ,@filtered-path)))))
	       (: erlang send to message)
	       (receive-loop ref)))))))))

(defun receive-loop (ref)
  (receive ((tuple ref pid 'ping)
	    (: erlang send pid 'pong)
	    (receive-loop ref))
	   ((tuple ref msg)
	    msg)))

(defun out (arg)
  (let* ((url (: yaws_api request_url arg))
	 (path (: string tokens (url-path url) '"/")))
    (out arg (http_request-method (arg-req arg)) path)))

(defmacro gen_resources [node resources]
  (: io format '"~p ~p ~n" (list node resources)) 
  (let ((resource_bodies (: lists map (lambda [X] (make-resource-bodies X node)) resources)))
    (: io format '"~p ~n" (list resource_bodies))
    `(defun out ,@resource_bodies
       ([arg method path]
	(: io format '"~p ~p~n" (list method path))
	(tuple 'status 404)))))

;(gen_resources lisp@jon-VirtualBox [add [N M]] [div [N M]] [mul [N M]] [sub [N M]])
