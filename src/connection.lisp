#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-gearman)

(defclass connection ()
  ((host
    :initarg  :host
    :initform "localhost"
    :reader   host)
   (port
    :initarg  :port
    :initform 4730
    :reader   port)
   (socket
    :initform nil
    :accessor socket)
   (stream
    :initform nil
    :accessor gm-stream)))

(define-condition gearman-connection-error (error) 
  ((message :initarg :message
            :initform nil)
   (comment :initarg :comment
            :initform nil)))

;;
;; macros borrowed from cl-redis
;;
(defmacro signal-connection-error-with-reconnect-restart
    (&key message comment restart)
  "Signal the condition of type GEARMAN-CONNECTION-ERROR denoted by the given
MESSAGE and COMMENT offering a :reconnect restart given by RESTART."
  `(restart-case (error 'gearman-connection-error
                        :message ,message
                        :comment ,comment)
     (reconnect ()
       :report "Try to reconnect."
       ,restart)
     (skip-connection ()
       :report "Skip connection attempt."
       nil)))

(defmacro provide-reconnect-restart (expression &body body)
  "When, during the execution of EXPRESSION, an error occurs that can break
the connection socket, a condition of type GEARMAN-CONNECTION-ERROR is raised
offering a :reconnect restart whose body is given by BODY."
  (alexandria:with-gensyms (err body-fn)
    `(flet ((,body-fn () ,@body))
       (handler-case ,expression
         (usocket:connection-refused-error (,err)
           ;; Errors of this type commonly occur when there is no Gearmand
           ;; running, or when one tries to connect to the wrong host or port.
           ;; We anticipate this by providing a helpful comment.
           (signal-connection-error-with-reconnect-restart
            :message ,err
            :comment "Make sure Gearmand is running and check your connection parameters."
            :restart (,body-fn)))
         ((or usocket:socket-error stream-error) (,err)
           (signal-connection-error-with-reconnect-restart
            :message ,err
            :restart (,body-fn)))))))

(defun reopen-connection (conn)
  "Close and reopen CONN."
  (gm-close conn)
  (gm-connect conn))

(defun open-connection-p (conn)
  (with-slots (stream) conn
    (and stream (open-stream-p stream))))

(defun ensure-connection (conn)
  "Ensure that CONN is open before doing anything with it."
  (unless conn
    (error "No Gearman connection specified."))
  (unless (open-connection-p conn)
    (signal-connection-error-with-reconnect-restart
     :message "Connection to Gearman server lost."
     :restart (reopen-connection conn))))

(defmacro with-reconnect-restart (conn &body body)
  "When, inside BODY, an error occurs that breaks the socket of CONN,
a condition of type REDIS-CONNECTION-ERROR is raised offering a :reconnect
restart."
  (alexandria:with-gensyms (=conn= =body=)
    `(let ((,=conn= ,conn))
       (ensure-connection ,=conn=)
       (labels ((,=body= ()
                  (provide-reconnect-restart (progn ,@body)
                    (reopen-connection ,=conn=)
                    (,=body=))))
         (,=body=)))))

(defmethod gm-connect ((conn connection))
  (provide-reconnect-restart
      (setf (socket conn) (usocket:socket-connect (host conn)
                                                  (port conn)
                                                  :protocol :stream
                                                  :element-type '(unsigned-byte 8))
            (gm-stream conn) (usocket:socket-stream (socket conn)))
    (gm-connect conn)))

(defmethod gm-close ((conn connection))
  (with-slots (socket stream) conn
    (if socket (usocket:socket-close socket))
    (setf socket nil)
    (setf stream nil)))

(defmethod initialize-instance :after ((conn connection) &key)
  (gm-connect conn))

(defun send-request (conn msg)
  (with-slots (stream) conn
    (write-message stream msg))
  (log-debug (format nil "sent request: ~a" msg)))

(defun recv-response (conn)
  (with-slots (stream) conn
    (let ((msg (read-message stream)))
      (log-debug (format nil "received response: ~a" msg))
      msg)))

(defmethod is-active ((conn connection))
  (handler-case (progn
                  (send-request conn (make-instance 'message :name ECHO_REQ :data (list "test-connection")))
                  (let ((msg (recv-response conn)))
                    (message-name? msg ECHO_RES)))
    (error (err)
      (log-debug err) nil)))
