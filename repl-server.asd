(asdf:defsystem #:repl-server
  :depends-on (:hunchentoot :bordeaux-threads :cl-json)
  :serial t
  :version "0.3.0"
  :components ((:file "termcolor")
               (:file "repl-server")))
