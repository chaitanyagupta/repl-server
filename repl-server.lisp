(cl:defpackage #:repl-server
  (:use #:cl #:hunchentoot)
  (:export #:start-server
           #:stop-server
           #:start-repl)
  (:shadow #:session #:*session*))

(cl:in-package #:repl-server)

;;; Utils

(defmacro if-let ((var test) then &optional else)
  `(let ((,var ,test))
     (if ,var
         ,then
         ,else)))

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defvar *sessions* (make-hash-table))
(defvar *sessions-lock* (bt:make-lock))

(defvar *session*)
(defvar *current-session*)

(defstruct (session (:conc-name %session-))
  (sid (random 1000000000) :read-only t)
  (rid)
  (lock (bt:make-lock) :read-only t)
  (condition-var (bt:make-condition-variable) :read-only t)
  (eval-src nil)
  (result nil))

(defmacro def-session-property (name accessor &key read-only)
  `(progn
     (defun ,name (&optional (session *session*))
       (,accessor session))
     ,(unless read-only
       `(defun (setf ,name) (value &optional (session *session*))
          (setf (,accessor session) value)))))

(def-session-property sid %session-sid :read-only t)
(def-session-property rid %session-rid)
(def-session-property session-lock %session-lock :read-only t)
(def-session-property session-condition-var %session-condition-var :read-only t)
(def-session-property eval-src %session-eval-src)
(def-session-property result %session-result)

(defun new-session ()
  (let ((session (make-session)))
    (bt:with-lock-held (*sessions-lock*)
      (setf (gethash (sid session) *sessions*) session
            *current-session* session))))

(defun get-session ()
  (let* ((sid (parse-integer (second (cl-ppcre:split "/" (request-uri*)))))
         (session (bt:with-lock-held (*sessions-lock*)
                    (gethash sid *sessions*))))
    (unless session
      (setf (return-code*) +http-not-found+)
      (abort-request-handler))
    session))

(define-easy-handler (start-handler :uri "/start")
    ()
  (let ((session (new-session)))
    (setf (content-type*) "application/json")
    (json:encode-json-to-string `((:sid . ,(sid session))))))

(define-easy-handler (stop-handler :uri (lambda (request) (cl-ppcre:scan "^/.*/stop$" (request-uri* request))))
    ()
  (let ((session (get-session)))
    (bt:with-lock-held (*sessions-lock*)
      (remhash (sid session) *sessions*))
    nil))

(define-easy-handler (eval-handler :uri (lambda (request) (cl-ppcre:scan "^/.*/eval$" (request-uri* request))))
    ()
  (let* ((*session* (get-session))
         (response-json (raw-post-data :force-text t))
         (response (json:decode-json-from-string response-json))
         (rid (dot response :rid))
         (result (dot response :result)))
    (setf (rid) rid)
    (when result
      (bt:with-lock-held ((session-lock))
        (setf (result) result)
        (bt:condition-notify (session-condition-var))))
    (bt:with-lock-held ((session-lock))
      (loop
         (bt:condition-wait (session-condition-var) (session-lock))
         (when (eval-src)
           (setf (content-type*) "application/json")
           (return (json:encode-json-to-string (list (cons :query (eval-src))))))))))

(defmethod handle-request :around ((acceptor acceptor) (request request))
  (setf (header-out :access-control-allow-origin) "*")
  (setf (header-out :access-control-allow-headers) (header-in* :access-control-request-headers))
  (unless (eql (request-method*) :options)
    (call-next-method)))

(defun eval-string (string &optional (*session* *current-session*))
  (bt:with-lock-held ((session-lock))
    (setf (eval-src) string)
    (bt:condition-notify (session-condition-var)))
  (bt:with-lock-held ((session-lock))
    (loop
       (bt:condition-wait (session-condition-var) (session-lock))
       (when (result)
         (return (prog1 (result)
                   (setf (eval-src) nil
                         (result) nil)))))))

(defvar *color-output*)

(defun fg (name)
  (termcolor:fg name :print *color-output*))

(defun bg (name)
  (termcolor:bg name :print *color-output*))

(defun style (name)
  (termcolor:style name :print *color-output*))

(defun reset ()
  (termcolor:reset :print *color-output*))

(defun start-repl (&key
                   ((:color-output *color-output*) nil)
                   exit-on-finish)
  (loop
     (style :bright)
     (princ "REPL> ")
     (reset)
     (force-output)
     (let ((to-eval (read-line)))
       (when (string-equal to-eval "//quit")
         (if exit-on-finish
             #+sbcl (sb-ext:quit)
             #-sbcl (return)
             (return)))
       (let* ((response (json:decode-json-from-string (eval-string to-eval)))
              (type (dot response :type))
              (value (dot response :value))
              (constructor (dot response :constructor)))
         (cond
           ((string-equal type "undefined")
            (style :dim)
            (princ "undefined")
            (reset))
           ((string-equal type "boolean")
            (if value (princ "true") (princ "false")))
           ((string-equal type "number")
            (princ value))
           ((string-equal type "string")
            (prin1 value))
           ((string-equal type "function")
            (princ "<function>")
            (when-let (name (dot response :name))
              (format t " ~A" name))
            (when-let (source (dot response :source))
              (terpri)
              (princ source)))
           ((string-equal type "object")
            (cond
              ((and (dot response :error) (dot response :thrown))
               (fg :red)
               (format t "ERROR! [~A] ~A" constructor (dot response :error :message))
               (reset))
              ((and (assoc :value response) (null (dot response :value)))
               (princ "null"))
              ((string-equal constructor "RegExp")
               (princ value))
              (t
               (princ "<object>")
               (when constructor
                (format t "/[~A]" constructor)))))
           (t (warn "Unrecognized object:~%~S" response)))
         (terpri)))))

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

