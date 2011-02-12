(asdf:defsystem #:repl-server
  :depends-on (:hunchentoot :bordeaux-threads :cl-json)
  :components ((:file "repl-server")))