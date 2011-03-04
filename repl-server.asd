(asdf:defsystem #:repl-server
  :depends-on (:hunchentoot
               :bordeaux-threads
               :cl-json
               :parse-js
               :trivial-gray-streams)
  :serial t
  :version "0.4.1"
  :components ((:module lib
                        :components ((:file "termcolor")))
               (:module src
                        :components ((:file "package")
                                     (:file "pipe")
                                     (:file "repl-server")))))
