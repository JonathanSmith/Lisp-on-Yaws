(ql:quickload "thread-pool")
(ql:quickload "cleric")

(defpackage lisp-on-yaws
  (:use :common-lisp)
  (:export 
   #:*yaws-server-node-name*
   #:*cookie-file*
   #:*pids-hash*
   #:reg-pids-hash*
   #:*thread-pool*
   #:hash-dispatch
   #:set-hash-dispatch
   #:init-server-connection
   #:register
   #:*elements*
   #:*type*
   #:*dest*
   #:*ref*
   #:*query*
   #:*reply-type*
   #:*content-type*
   #:write-module-string
   #:send-file-for-compilation
   #:send-static-page
   #:add-appmod
   #:reply
   #:easy-handler
   #:static-page-generator
   #:parse-query
   #:parse-multipart-query
   #:*appmods*
   #:add-appmod-resource
   #:defhandler
   #:generate-appmods))