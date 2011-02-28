(cl:defpackage #:repl-server
  (:use #:cl #:hunchentoot)
  (:export #:start-server
           #:stop-server
           #:start-repl
           #:defcolor)
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

(defun read-file (f)
  (with-output-to-string (out)
    (loop
       for line = (read-line f nil)
       while line
       do (write-line line out))))

;;; Server

(defvar *version* (asdf:component-version (asdf:find-system :repl-server)))

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
  (eval-semaphore (bt:make-condition-variable) :read-only t)
  (result-semaphore (bt:make-condition-variable) :read-only t)
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
(def-session-property eval-semaphore %session-eval-semaphore :read-only t)
(def-session-property result-semaphore %session-result-semaphore :read-only t)
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
         (version (dot response :version)))
    (unless (string= version *version*)
      (notify :warn "Client version (~A) differs from server version (~A). The REPL may not work."
              version *version*))
    (setf (content-type*) "application/json")
    (prog1
        (json:encode-json-to-string `((:sid . ,(sid session))))
      (notify :info "New client connected. sid: ~A" (sid session)))))

(define-easy-handler (stop-handler :uri (lambda (request) (cl-ppcre:scan "^/.*/stop$" (request-uri* request))))
    ()
  (let ((session (get-session (request-uri*))))
    (bt:with-lock-held (*sessions-lock*)
      (remhash (sid session) *sessions*)
      (when (eql *current-session* session)
        (makunbound '*current-session*)))
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
        (bt:condition-notify (result-semaphore))))
    (setf (content-type*) "application/json")
    (handler-case
        (bt:with-timeout (*timeout*)
          (bt:with-lock-held ((session-lock))
            (loop
               (bt:condition-wait (eval-semaphore) (session-lock))
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

(defun eval-string (string &optional (*session* *current-session*))
  (bt:with-lock-held ((session-lock))
    (setf (eval-src) string)
    (bt:condition-notify (eval-semaphore)))
  (bt:with-lock-held ((session-lock))
    (loop
       (bt:condition-wait (result-semaphore) (session-lock))
       (when-let (result (result))
         (setf (eval-src) nil
               (result) nil)
         (return result)))))

;;; REPL

(defvar *color-output*)

(defun color (&key fg bg style)
  (termcolor:color :fg fg :bg bg :style style :print *color-output*))

(defun reset ()
  (termcolor:reset :print *color-output*))

(defvar *colors* (make-hash-table))

(defun defcolor (keyword &key fg bg style)
  (setf (gethash keyword *colors*) (list :fg fg :bg bg :style style)))

(defun apply-color (keyword)
  (let ((args (gethash keyword *colors*)))
    (if args
        (apply #'color args)
        (error "Unrecognized color keyword: ~A" keyword))))

;;;; Default colors

(defcolor :repl :style :bright)
(defcolor :result :fg :reset)
(defcolor :info :fg :reset)
(defcolor :warn :fg :yellow)
(defcolor :error :fg :red)

(defvar *repl-stream*)
(defvar *repl-stream-lock* (bt:make-lock))

(defmacro with-repl-stream-lock (&body body)
  `(bt:with-lock-held (*repl-stream-lock*)
     ,@body))

(defun notify (level fmt &rest args)
  (let ((*standard-output* *repl-stream*))
    (with-repl-stream-lock
      (fresh-line)
      (ecase level
        ((:info :warn :error) (apply-color level)))
      (apply #'format t fmt args)
      (reset)
      (terpri))))

(defun prompt ()
  (let ((*standard-output* *repl-stream*))
    (with-repl-stream-lock
      (apply-color :repl)
      (princ "REPL> ")
      (reset)
      (force-output))))

(defvar *exit-on-finish*)

(defun quit ()
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit)
  #-(or sbcl ccl) (error "Don't know how to quit!"))

(defvar *repl-commands* (make-hash-table :test #'equal))

(defmacro define-repl-command (name args &body body)
  `(setf (gethash ,name *repl-commands*) (lambda ,args ,@body)))

(define-repl-command "quit" ()
  (if *exit-on-finish*
      (quit)
      (throw 'done nil)))

(define-repl-command "version" ()
  (princ *version*)
  (terpri))

(define-repl-command "pwd" ()
  (princ (namestring (probe-file *default-pathname-defaults*)))
  (terpri))

(define-repl-command "cd" (dir)
  (let ((new-pathname (probe-file (merge-pathnames (pathname dir)
                                                   (probe-file *default-pathname-defaults*)))))
    (if new-pathname
        (progn
          (setf *default-pathname-defaults* new-pathname)
          (princ *default-pathname-defaults*))
        (progn
          (apply-color :error)
          (princ "Invalid directory."))))
  (terpri))

(define-repl-command "load" (file)
  (with-open-file (f file :direction :input)
    (eval-string (read-file f))))

(defun commandp (string)
  (string-equal (subseq string 0 (min 2 (length string))) "//"))

(defun parse-command (string)
  (with-input-from-string (in (subseq string 2))
    (flet ((till-next-whitespace ()
             (with-output-to-string (out)
               (loop
                  for char = (read-char in nil)
                  while char
                  if (char= char #\Space)
                  do (return)
                  else do (write-char char out)))))
      (let ((command (till-next-whitespace))
            (args))
        (loop
           for char = (peek-char t in nil)
           while char
           if (char= char #\")
           do (push (read in nil) args)
           else if (char/= char #\Space)
           do (push (till-next-whitespace) args))
        (setf args (nreverse args))
        (values command args)))))

(defun read-js (stream)
  (let ((string "")
        (eof-line -1)
        (eof-column))
    (loop
       (let ((line (read-line stream nil)))
         (when (null line)
           (return))
         (setf string (format nil "~A~&~A" string line))
         (incf eof-line)
         (setf eof-column (length line))
         (unless (eql (check-js string eof-line eof-column) :continue)
           (return))))
    string))

(defun check-js (string eof-line eof-column)
  (handler-bind ((parse-js:js-parse-error
                  #'(lambda (c)
                      ;; A couple of bugs in parse-js: 1) an internal symbol is
                      ;; not exported, and 2) inconsistent line and column
                      ;; number indexing by js-parse-error
                      (let ((line (1- (parse-js:js-parse-error-line c)))
                            (column (parse-js::js-parse-error-char c)))
                        (when (and (= line eof-line) (= eof-column column))
                          (return-from check-js :continue))))))
    (parse-js:parse-js string :strict-semicolons t)))

(defun rep (&aux (*standard-output* (two-way-stream-output-stream *repl-stream*)))
  (prompt)
  (let ((to-eval (handler-case (read-js *repl-stream*)
                   (parse-js:js-parse-error (c)
                     ;; another fix for parse-js's inconsistent indexing
                     (incf (slot-value c 'parse-js::char))
                     (with-repl-stream-lock
                       (apply-color :error)
                       (format t "JS Parse Error: ~A" c)
                       (reset)
                       (terpri))
                     (throw 'continue nil)))))
    (when (commandp to-eval)
      (multiple-value-bind (command args)
          (parse-command to-eval)
        (if-let (fn (gethash command *repl-commands*))
          (apply fn args)
          (with-repl-stream-lock
            (apply-color :error)
            (princ "Invalid command.")
            (reset)
            (terpri))))
      (throw 'continue nil))
    (unless (boundp '*current-session*)
      (with-repl-stream-lock
        (apply-color :warn)
        (princ "No client connected.")
        (reset)
        (terpri))
      (throw 'continue nil))
    (let* ((response-string (eval-string to-eval))
           (response (json:decode-json-from-string response-string))
           (type (dot response :type))
           (value (dot response :value))
           (constructor (dot response :constructor)))
      (with-repl-stream-lock
        (cond
          ((string-equal type "Undefined")
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
          ((string-equal type "Array")
           (json:encode-json value *standard-output*))
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
                 (apply-color :error)
                 (format t "ERROR! Type: ~A~%~A" constructor (dot response :error))
                 (reset))
               (format t "<Error> Type: ~A~%~A" constructor (dot response :error))))
          ((string-equal type "Object")
           (princ "<object>")
           (when constructor
             (format t "/[~A]" constructor)))
          (t
           (apply-color :error)
           (format t "Unrecognized object:~%~S" response-string)
           (reset)))
        (terpri)))))

(defun start-repl (&key
                   color-output
                   exit-on-finish
                   (output-stream *standard-output*)
                   (input-stream *standard-input*))
  (setf *exit-on-finish* exit-on-finish
        *color-output* color-output
        *repl-stream* (make-two-way-stream input-stream output-stream))
  (catch 'done
    (loop
       (catch 'continue
         (with-simple-restart (repl-prompt "Ignore error and get REPL prompt.")
           (rep))))))

(defun dot (object key &rest more-keys)
  (let ((value (cdr (assoc key object))))
    (if more-keys
        (apply #'dot value more-keys)
        value)))



