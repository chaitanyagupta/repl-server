(cl:defpackage #:repl-server
  (:use #:cl #:hunchentoot)
  (:export #:start-server
           #:stop-server
           #:start-repl
           #:defcolor)
  (:shadow #:session #:*session*))
