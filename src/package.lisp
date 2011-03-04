(cl:defpackage #:repl-server
  (:use #:cl #:hunchentoot #:trivial-gray-streams)
  (:export #:start-server
           #:stop-server
           #:start-repl
           #:defcolor)
  (:shadow #:session #:*session*))
