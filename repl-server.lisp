(cl:defpackage #:repl-server
  (:use #:cl #:hunchentoot)
  (:export #:start-server
           #:stop-server))

(cl:in-package #:repl-server)

(defvar *eval-condition-var* (bt:make-condition-variable))
(defvar *eval-lock* (bt:make-lock))
(defvar *eval-string* nil)
(defvar *result-string* nil)

(defun eval-string (string)
  (bt:with-lock-held (*eval-lock*)
    (setf *eval-string* string)
    (bt:condition-notify *eval-condition-var*))
  (bt:with-lock-held (*eval-lock*)
    (loop
       (bt:condition-wait *eval-condition-var* *eval-lock*)
       (when *result-string*
         (return (prog1 (json:decode-json-from-string *result-string*)
                   (setf *eval-string* nil
                         *result-string* nil)))))))

(define-easy-handler (eval-string-handler :uri "/eval-string")
    ((result :parameter-type 'string))
  (bt:with-lock-held (*eval-lock*)
    (setf *result-string* result)
    (bt:condition-notify *eval-condition-var*))
  (bt:with-lock-held (*eval-lock*)
    (loop
       (bt:condition-wait *eval-condition-var* *eval-lock*)
       (setf (header-out :access-control-allow-origin) "*")
       (setf (content-type*) "text/javascript")
       (when *eval-string*
         (return *eval-string*)))))

(defun eval-loop ()
  (loop
     (princ "REPL> ")
     (let ((to-eval (read-line)))
       (when (string-equal to-eval ";quit")
         (return))
       (let* ((result (eval-string to-eval))
              (constructor (dot result :constructor))
              (value (dot result :value)))
         (format t "<~A>" (dot result :type))
         (when constructor
           (format t "/[~A]" constructor))
         (terpri)
         (when value
           (json:encode-json value)
           (terpri))))))

(defun dot (object key &rest more-keys)
  (let ((value (cdr (assoc key object))))
    (if more-keys
        (apply #'dot value more-keys)
        value)))

(defvar *server*)

(defun start-server (&optional (port 8000))
  (setf *server* (start (make-instance 'acceptor :port port))))

(defun stop-server ()
  (stop *server*))

