(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload "CL-WHO")
  (ql:quickload "parenscript")
  (ql:quickload "cl-ppcre")
  (ql:quickload "cl-markdown")
  (ql:quickload "uuid")
  (ql:quickload "cl-json")
  (ql:quickload "cl-sendmail")
  (ql:quickload "cl-redis")
  (asdf:operate 'asdf:load-op :lisp-on-yaws))

(defpackage blog
  (:use :lisp-on-yaws :common-lisp)
  (:export #:blog-main))