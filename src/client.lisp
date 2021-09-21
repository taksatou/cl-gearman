#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-gearman)

(defclass client ()
  ((connections
    :initarg :connections
    :initform nil)
   (tasks
    :initform nil)
   ))

(defun make-client (server-exps)
  (let ((client (make-instance 'client)))
    (with-slots (connections) client
      (setf connections (loop for server in server-exps
                           collect (let* ((lis (split-sequence:split-sequence #\: server))
                                          (host (car lis))
                                          (port (cadr lis)))
                                     (if port
                                         (make-instance 'connection
                                                        :host host
                                                        :port (parse-integer port))
                                         (make-instance 'connection :host host))))))
    client))

(defmethod select-server ((cli client) uid)
  "Select the first server ready to response, if some connection error appears the server is
automatically discarded"
  (with-slots (connections) cli
    (find-if
     (lambda (conn)
       (handler-case (progn
                       (send-request conn (make-instance 'message :name ECHO_REQ :data (list "test-connection")))
                       (let ((msg (recv-response conn)))
                         (message-name? msg ECHO_RES)))
         (error (err)
           (log-debug err) nil)))
     connections)))

(define-condition job-failed (error)
  ((response :initarg :response
             :reader server-response))
  (:report (lambda (condition stream)
             (format stream "job failed: ~A"
                     (server-response condition)))))

(defmethod submit-job ((cli client) func &key arg priority)
  "Submit a job and return result data of the job. This function blocks until it is completed."
  (let* ((uid (make-uid))
         (conn (select-server cli uid))
         (type (cond ((null priority) SUBMIT_JOB)
                     ((eql priority :high) SUBMIT_JOB_HIGH)
                     ((eql priority :low) SUBMIT_JOB_LOW))))
    (with-reconnect-restart conn
      (send-request conn (make-instance 'message :name type :data (list func uid arg)))
      (loop for msg = (recv-response conn)
            do (cond ((message-name? msg JOB_CREATED)
                      ;; nothing todo
                      ;; (log-debug (format nil "job created: ~A" msg))
                      )
                     ((message-name? msg WORK_COMPLETE)
                      ;; (log-debug (format nil "job complete: ~A" msg))
                      (return (cadr (slot-value msg 'data))))
                     ((message-name? msg WORK_FAIL)
                      (restart-case (error 'job-failed :response msg)
                        (retry-job ()
                          :report (lambda (stream) (format stream "Retry the job."))
                          (submit-job cli func :arg arg :priority priority))
                        (accept-job ()
                          :report (lambda (stream) (format stream "Continue, treating the job has successed."))
                          (return nil))))
                     (t
                      (log-warn (format nil "not implemented yet. ~A" msg))))))))

(defmethod submit-background-job ((cli client) func &key arg priority)
  "Submit job and return the job object immediately."
  (let* ((uid (make-uid))
         (conn (select-server cli uid))
         (type (cond ((null priority) SUBMIT_JOB_BG)
                     ((eql priority :high) SUBMIT_JOB_HIGH_BG)
                     ((eql priority :low) SUBMIT_JOB_LOW_BG))))
    (with-reconnect-restart conn
      (send-request conn (make-instance 'message :name type :data (list func uid arg)))
      (let* ((msg (recv-response conn)))
        (if (message-name? msg JOB_CREATED)
            (progn
                                        ;            (log-debug (format nil "job created: ~A" msg))
              (job-created-message-to-job msg :name type :arg arg :uid uid))
            (restart-case (error (format nil "failed to create job: ~A" msg))
              (retry-job ()
                :report (lambda (stream) (format stream "Retry submitting the job."))
                (submit-background-job cli func :arg arg :priority priority))))))))

(defmethod get-job-status ((cli client) (j job))
  (with-slots (uid handle) j
    (let ((conn (select-server cli uid)))
      (with-reconnect-restart conn
        (send-request conn (make-instance 'message :name GET_STATUS :data (list handle)))
        (let* ((msg (recv-response conn))
               (data (slot-value msg 'data))
               (job-handle (car data))
               (known-status (cadr data))
               (running-status (caddr data))
               (numerator (parse-integer (cadddr data)))
               (denominator (parse-integer (car (cddddr data)))))
          (list :job-handle job-handle
                :is-known known-status
                :is-running running-status
                :progress (if (> denominator 0) (/ numerator denominator) :nan)))))))


(defmethod close-client ((c client))
  (with-slots (connections) c
    (loop for conn in connections do (gm-close conn))))

(defmacro with-client ((var host) &body body)
  `(let ((,var (make-client (list ,host))))
     (unwind-protect
          (progn ,@body)
       (close-client ,var))))

(defmacro with-multiple-servers-client ((var hosts) &body body)
  `(let ((,var (make-client ,hosts)))
     (unwind-protect
          (progn ,@body)
       (close-client ,var))))
