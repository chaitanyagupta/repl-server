(asdf:defsystem #:repl-server
  :depends-on (:hunchentoot :bordeaux-threads :cl-json)
  :serial t
  :components ((:file "termcolor")
               (:file "repl-server")))
