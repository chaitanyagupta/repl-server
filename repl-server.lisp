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

;;; Server

(defvar *sessions* (make-hash-table :test #'equal))
(defvar *sessions-lock* (bt:make-lock))

(defvar *session*)
(defvar *current-session*)

(defvar *timeout* 120)

(defun get-sid ()
  (format nil "~A~A" (get-universal-time) (random 100000000000)))

(defstruct (session (:conc-name %session-))
  (sid (get-sid) :read-only t)
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

(defun get-session (uri)
  (let* ((sid (second (cl-ppcre:split "/" uri)))
         (session (bt:with-lock-held (*sessions-lock*)
                    (gethash sid *sessions*))))
    (unless session
      (setf (return-code*) +http-not-found+)
      (abort-request-handler))
    session))

(define-easy-handler (start-handler :uri "/start")
    ()
  (let* ((session (new-session))
         (response-json (raw-post-data :force-text t))
         (response (json:decode-json-from-string response-json))
         (version (dot response :version))
         (asdf-version (asdf:component-version (asdf:find-system :repl-server))))
    (unless (string= version asdf-version)
      (warn "Client version (~A) differs from server version (~A). The REPL may not work."
            version asdf-version))
    (setf (content-type*) "application/json")
    (prog1
        (json:encode-json-to-string `((:sid . ,(sid session))))
      (repl-stream-message "~&New client connected. sid: ~A~%" (sid session)))))

(define-easy-handler (stop-handler :uri (lambda (request) (cl-ppcre:scan "^/.*/stop$" (request-uri* request))))
    ()
  (let ((session (get-session (request-uri*))))
    (bt:with-lock-held (*sessions-lock*)
      (remhash (sid session) *sessions*))
    nil))

(define-easy-handler (eval-handler :uri (lambda (request) (cl-ppcre:scan "^/.*/eval$" (request-uri* request))))
    ()
  (let* ((*session* (get-session (request-uri*)))
         (response-json (raw-post-data :force-text t))
         (response (json:decode-json-from-string response-json))
         (rid (dot response :rid))
         (result (dot response :result)))
    (setf (rid) rid)
    (when result
      (bt:with-lock-held ((session-lock))
        (setf (result) result)
        (bt:condition-notify (session-condition-var))))
    (setf (content-type*) "application/json")
    (handler-case
        (bt:with-timeout (*timeout*)
          (bt:with-lock-held ((session-lock))
            (loop
               (bt:condition-wait (session-condition-var) (session-lock))
               (when (eval-src)
                 (return (json:encode-json-to-string (list (cons :query (eval-src)))))))))
      (bt:timeout () (json:encode-json-to-string (list (cons :timeout t)))))))

(defmethod handle-request :around ((acceptor acceptor) (request request))
  (setf (header-out :access-control-allow-origin) "*")
  (setf (header-out :access-control-allow-headers) (header-in* :access-control-request-headers))
  (unless (eql (request-method*) :options)
    (call-next-method)))

(defvar *server*)

(defun start-server (&optional (port 8000))
  (setf *server* (start (make-instance 'acceptor :port port))))

(defun stop-server ()
  (stop *server*))

;;; REPL

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

(defvar *repl-stream-lock* (bt:make-lock))

(defmacro with-repl-stream-lock (&body body)
  `(bt:with-lock-held (*repl-stream-lock*)
     ,@body))

(defun repl-stream-message (fmt &rest args)
  (with-repl-stream-lock
    (apply #'format t fmt args)))

(define-condition syntax-error (error)
  ((message :reader syntax-error-message
            :initarg :message))
  (:report (lambda (condition stream)
             (write-string (syntax-error-message condition) stream))))

(defun syntax-error (fmt &rest args)
  (error 'syntax-error :message (apply #'format nil fmt args)))

(defparameter *string-delimiters*
  '(#\" #\' #\/))

(defparameter *syntax-delimiter-pairs*
  '((#\( . #\))
    (#\{ . #\})
    (#\[ . #\])))

(defparameter *block-delimiter-pair*
  '(#\{ . #\}))

(defparameter *terminator*
  #\;)

(defparameter *escape-char*
  #\\)

(defparameter *whitespace-chars*
  '(#\Space #\Tab #\Newline))

(defun string-delimiter-p (char)
  (member char *string-delimiters*))

(defun syntax-delimiter-start-p (char)
  (member char (mapcar #'car *syntax-delimiter-pairs*)))

(defun syntax-delimiter-end-p (char)
  (member char (mapcar #'cdr *syntax-delimiter-pairs*)))

(defun syntax-delimiter-complement (char)
  (cond
    ((syntax-delimiter-start-p char)
     (cdr (assoc char *syntax-delimiter-pairs*)))
    ((syntax-delimiter-end-p char)
     (car (rassoc char *syntax-delimiter-pairs*)))
    (t nil)))

(defun block-delimiter-start-p (char)
  (eql char (car *block-delimiter-pair*)))

(defun block-delimiter-end-p (char)
  (eql char (cdr *block-delimiter-pair*)))

(defun terminatorp (char)
  (eql char *terminator*))

(defun escape-char-p (char)
  (eql char *escape-char*))

(defun whitespacep (char)
  (member char *whitespace-chars*))

(defun read-command (&optional stream)
  (let* ((first-char (read-char stream nil nil))
         (second-char (and (eql first-char #\/)
                           (peek-char nil stream nil nil))))
    (if (and (eql first-char #\/) (eql first-char second-char))
        (with-output-to-string (out)
          (write-char first-char out)
          (write-string (read-line stream) out))
        (unread-char first-char stream))))

(defun read-javascript (&optional stream)
  (let ((states '())
        (current nil)
        (last-nw-char nil)) ; last-non-whitespace-char
    (flet ((push-state (state &optional char)
             (setf current (cons state char))
             (push current states))
           (pop-state ()
             (prog1 (pop states)
               (setf current (car states))))
           (current-state ()
             (car current))
           (current-delimiter ()
             (cdr current))
           (level ()
             (1- (length states))))
      (push-state :syntax)
      (with-output-to-string (out)
        (loop
           (let ((char (read-char stream nil nil)))
             (cond
               ((eql char #\Newline)
                (when (and (eql (current-state) :syntax) (zerop (level)))
                  (if (or (terminatorp last-nw-char) (block-delimiter-end-p last-nw-char))
                      (return)
                      (syntax-error "Missing semicolon."))))
               ((escape-char-p char)
                (if (not (eql (current-state) :escape))
                    (push-state :escape)
                    (pop-state)))
               ((string-delimiter-p char)
                (case (current-state)
                  (:escape (pop-state))
                  (:syntax (push-state :string char))
                  (:string (when (eql (current-delimiter) char)
                             (pop-state)))))
               ((syntax-delimiter-start-p char)
                (case (current-state)
                  (:escape (pop-state))
                  (:syntax (push-state :syntax char))))
               ((syntax-delimiter-end-p char)
                (case (current-state)
                  (:escape (pop-state))
                  (:syntax (if (eql (current-delimiter) (syntax-delimiter-complement char))
                               (pop-state)
                               (syntax-error "Invalid token: ~A" char)))))
               (t
                (case (current-state)
                  (:escape (pop-state)))))
             (unless (whitespacep char)
               (setf last-nw-char char))
             (write-char char out)))))))

(defun prompt ()
  (with-repl-stream-lock
    (style :bright)
    (princ "REPL> ")
    (reset)
    (force-output)))

(defun rep (*color-output* exit-on-finish)
  (prompt)
  (let ((to-eval (handler-case (or (read-command) (read-javascript))
                   (syntax-error (c)
                     (with-repl-stream-lock
                       (princ c)
                       (terpri))
                     (throw 'continue nil)))))
    (when (string-equal to-eval "//quit")
      (if exit-on-finish
          #+sbcl (sb-ext:quit)
          #-sbcl (return)
          (throw 'done nil)))
    (unless (boundp '*current-session*)
      (warn "No client connected.")
      (throw 'continue nil))
    (let* ((response-string (eval-string to-eval))
           (response (json:decode-json-from-string response-string))
           (type (dot response :type))
           (value (dot response :value))
           (constructor (dot response :constructor)))
      (with-repl-stream-lock
        (cond
          ((string-equal type "Undefined")
           (style :dim)
           (princ "undefined")
           (reset))
          ((string-equal type "Null")
           (princ "null"))
          ((string-equal type "Boolean")
           (if value (princ "true") (princ "false")))
          ((string-equal type "Number")
           (princ value))
          ((string-equal type "String")
           (prin1 value))
          ((string-equal type "RegExp")
           (princ value))
          ((string-equal type "Function")
           (princ "<function>")
           (when-let (name (dot response :name))
             (format t " ~A" name))
           (when-let (source (dot response :source))
             (terpri)
             (princ source)))
          ((string-equal type "Error")
           (if (dot response :thrown)
               (progn
                 (fg :red)
                 (format t "ERROR! Type: ~A~%~A" constructor (dot response :error))
                 (reset))
               (format t "<Error> Type: ~A~%~A" constructor (dot response :error))))
          ((string-equal type "Object")
           (princ "<object>")
           (when constructor
             (format t "/[~A]" constructor)))
          (t (warn "Unrecognized object:~%~S" response-string)))
        (terpri)))))

(defun start-repl (&key
                   ((:color-output *color-output*) nil)
                   exit-on-finish)
  (catch 'done
    (loop
       (catch 'continue
         (with-simple-restart (repl-prompt "Ignore error and get REPL prompt.")
           (rep *color-output* exit-on-finish))))))

(defun dot (object key &rest more-keys)
  (let ((value (cdr (assoc key object))))
    (if more-keys
        (apply #'dot value more-keys)
        value)))



