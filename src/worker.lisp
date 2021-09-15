#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-gearman)

(defclass worker ()
  ((connections
    :initarg :connections
    :initform nil)
   (abilities
    :initform (make-hash-table :test #'equalp)
    :accessor abilities)
   (status
    :initform :preparing
    :reader status
   )))

(defun make-worker (server-exps)
  (let ((worker (make-instance 'worker)))
    (with-slots (connections) worker
      (setf connections (loop for server in server-exps
                           collect (let* ((lis (split-sequence:split-sequence #\: server))
                                          (host (car lis))
                                          (port (cadr lis)))
                                     (if port
                                         (make-instance 'connection
                                                        :host host
                                                        :port (parse-integer port))
                                         (make-instance 'connection :host host))))))
    worker))

(defmethod select-server ((w worker) (j job))
  "Select the first server ready to response, if some connection error appears the server is
automatically discarded"
  (with-slots (connections) w
    (find-if
     (lambda (conn)
       (is-active conn))
     connections)))

(defmethod tell-job-fail ((w worker) (j job))
    (with-slots (handle) j
      (let ((conn (select-server w j)))
        (send-request conn (make-instance 'message :name WORK_FAIL :data (list handle))))))

(defmethod tell-job-success ((w worker) (j job) data)
    (with-slots (handle) j
      (let ((conn (select-server w j)))
        (send-request conn (make-instance 'message :name WORK_COMPLETE :data (list handle data))))))

(defmethod run-job ((w worker) (j job))
  (with-slots (abilities) w
    (with-slots (name arg) j
      (let ((fun (gethash name abilities)))
        (restart-case (tell-job-success w j (format nil "~A" (funcall fun arg j)))
          (retry-job ()
            :report (lambda (stream) (format stream "Retry the job. ~a" j))
            (run-job w j))
          (abort-job ()
            :report (lambda (stream) (format stream "Abort the job and tell job-server failure. ~a" j))
            (tell-job-fail w j))
          (skip-job (&optional (return-value ""))
            :report (lambda (stream) (format stream "Skip the job and tell job-server success. ~a" j))
            (tell-job-success w j return-value)))))))

(defmethod handle-job-assign ((w worker) (msg message))
  (let ((j (job-assign-message-to-job msg)))
    (run-job w j)))

(defmethod add-ability ((w worker) name func &key (timeout nil))
  "Announce an ability to servers and set the handler function."
  (with-slots (abilities connections) w
    (let ((cando-message (if timeout
                             (make-instance 'message :name CAN_DO_TIMEOUT :data (list name (format nil "~d" timeout)))
                             (make-instance 'message :name CAN_DO :data (list name)))))
      (loop for conn in connections
         do (and (is-active conn)
                 (with-reconnect-restart conn
                   (send-request conn cando-message)))))
    (setf (gethash name abilities) func) nil))

(defmethod remove-ability ((w worker) name)
  "Let job servers know that we're no longer able to do something."
  (with-slots (abilities connections) w
    (let ((cantdo-message (make-instance 'message :name CANT_DO :data (list name))))
      (loop for conn in connections
         do (and (is-active conn)
                 (with-reconnect-restart conn
                   (send-request conn cantdo-message)))))
    (remhash name abilities)))

(defmethod reset-abilities ((w worker))
  (with-slots (abilities connections) w
    (let ((reset-message (make-instance 'message :name RESET_ABILITIES)))
      (loop for conn in connections
         do (and (is-active conn)
                 (with-reconnect-restart conn
                   (send-request conn reset-message)))))
    (setf abilities (make-hash-table :test #'equalp))))

(defmethod work ((w worker))
  "Do a single job and return"
  (flet ((grab-job (conn)
           (loop do (progn
                      (send-request conn (make-instance 'message :name PRE_SLEEP))
                      (recv-response conn) ;should recieve noop
                      (send-request conn (make-instance 'message :name GRAB_JOB))
                      (let ((res (recv-response conn)))
                        (if (message-name? res JOB_ASSIGN) (return res))))))) ; can be NO_JOB
    (let ((conn (select-server w (make-instance 'job :handle "" :name "" :arg nil)))) ; dummy job
      ;; TODO : retry
      ;;   To support retry, add-ability is needed. with-reconnect-restart is not enough...
      ;;
      ;; When previous job was timed out on the server,
      ;; it will respond ERROR on our ECHO_REQ in `is-active` and
      ;; `select-server` will might return NIL.
      ;; That is why we need to run `grab-job` only
      ;; if connection was found:
      (when conn
        (let ((job (grab-job conn)))
          (handle-job-assign w job))))))

(defmethod close-worker ((w worker))
  (with-slots (connections) w
    (loop for conn in connections do (gm-close conn))))

(defmacro with-worker ((var host) &body body)
  `(let ((,var (make-worker (list ,host))))
     (unwind-protect
          (progn ,@body)
       (close-worker ,var))))

(defmacro with-multiple-servers-worker ((var hosts) &body body)
  `(let ((,var (make-worker ,hosts)))
     (unwind-protect
          (progn ,@body)
       (close-worker ,var))))
