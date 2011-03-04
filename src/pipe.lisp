(cl:in-package #:repl-server)

(defclass buffer ()
  ((store :initform "" :accessor store)
   (line-number :initform 1 :accessor line-number)
   (column-number :initform 0 :accessor column-number)
   (openp :initform t :reader openp)))

(defun close-buffer (buffer)
  (setf (slot-value buffer 'openp) nil))

(defclass pipe-stream ()
  ((buffer :initarg :buffer)
   (lock :initarg :lock)
   (waitqueue :initarg :waitqueue)))

(defun clear (x)
  (with-slots (lock buffer)
      x
    (bt:with-lock-held (lock)
      (setf (store buffer) ""))))

(defmethod close ((x pipe-stream) &key abort)
  (declare (ignore abort))
  (with-slots (lock waitqueue buffer)
      x
    (bt:with-lock-held (lock)
      (close-buffer buffer)
      (bt:condition-notify waitqueue))))

(defmethod open-stream-p ((x pipe-stream))
  (with-slots (lock buffer)
      x
    (bt:with-recursive-lock-held (lock)
      (openp buffer))))

(defclass pipe-output-stream (pipe-stream fundamental-character-output-stream)
  ())

(defmethod stream-clear-output ((x pipe-output-stream))
  (clear x))

(defmethod stream-write-char ((x pipe-output-stream) char)
  (unless (open-stream-p x)
    (error 'stream-error :stream x))
  (with-slots (lock waitqueue buffer)
      x
    (bt:with-lock-held (lock)
      (bt:condition-notify waitqueue)
      (setf (store buffer) (concatenate 'string (store buffer) (string char)))
      (if (eql char #\Newline)
          (progn (incf (line-number buffer)) (setf (column-number buffer) 0))
          (incf (column-number buffer))))))

(defmethod stream-line-column ((x stream))
  (with-slots (lock column)
      x
    (bt:with-lock-held (lock) column)))

(defclass pipe-input-stream (pipe-stream fundamental-character-input-stream)
  ())

(defmethod stream-clear-input ((x pipe-input-stream))
  (clear x))

(defmethod stream-read-char ((x pipe-input-stream))
  (with-slots (lock waitqueue buffer)
      x
    (bt:with-lock-held (lock)
      (when (zerop (length (store buffer)))
        (unless (open-stream-p x)
          (return-from stream-read-char :eof))
        (bt:condition-wait waitqueue lock))
      (unless (open-stream-p x)
        (return-from stream-read-char :eof))
      (prog1
          (char (store buffer) 0)
        (setf (store buffer) (subseq (store buffer) 1))))))

(defmethod stream-unread-char ((x pipe-input-stream) char)
  (with-slots (lock buffer)
      x
    (setf (store buffer) (concatenate 'string (string char) (store buffer)))))

(defmethod stream-listen ((x pipe-input-stream))
  (with-slots (lock buffer)
      x
    (bt:with-lock-held (lock)
      (plusp (length (store buffer))))))

(defun make-pipe ()
  (let* ((buffer (make-instance 'buffer))
         (lock (bt:make-lock))
         (waitqueue (bt:make-condition-variable)))
    (list (make-instance 'pipe-input-stream
                         :buffer buffer
                         :lock lock
                         :waitqueue waitqueue)
          (make-instance 'pipe-output-stream
                         :buffer buffer
                         :lock lock
                         :waitqueue waitqueue))))

(defun input-stream (pipe)
  (first pipe))

(defun output-stream (pipe)
  (second pipe))

