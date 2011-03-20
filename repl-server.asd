(asdf:defsystem #:repl-server
  :depends-on (:hunchentoot :bordeaux-threads :cl-json :parse-js)
  :serial t
  :version "0.5.0"
  :components ((:module lib
                        :components ((:file "termcolor")))
               (:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "repl-server")))))
