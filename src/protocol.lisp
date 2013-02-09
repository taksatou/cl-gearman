#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-gearman)

;; http://gearman.org/doku.php?id=protocol

(defconstant CAN_DO 1)  ;;              REQ    Worker
(defconstant CANT_DO 2)  ;;             REQ    Worker
(defconstant RESET_ABILITIES 3)  ;;     REQ    Worker
(defconstant PRE_SLEEP 4)  ;;           REQ    Worker
;; (defconstant    (unused) #(0 0 0 5))  ;;            -      -
(defconstant NOOP 6)  ;;                RES    Worker
(defconstant SUBMIT_JOB 7)  ;;          REQ    Client
(defconstant JOB_CREATED 8)  ;;         RES    Client
(defconstant GRAB_JOB 9)  ;;            REQ    Worker
(defconstant NO_JOB 10)  ;;              RES    Worker
(defconstant JOB_ASSIGN 11)  ;;          RES    Worker
(defconstant WORK_STATUS 12)  ;;         REQ    Worker
                                         ;;         RES    Client
(defconstant WORK_COMPLETE 13)  ;;       REQ    Worker
                                           ;;       RES    Client
(defconstant WORK_FAIL 14)  ;;           REQ    Worker
                                       ;;  RES    Client
(defconstant GET_STATUS 15)  ;;          REQ    Client
(defconstant ECHO_REQ 16)  ;;            REQ    Client/Worker
(defconstant ECHO_RES 17)  ;;            RES    Client/Worker
(defconstant SUBMIT_JOB_BG 18)  ;;       REQ    Client
(defconstant ERROR 19)  ;;               RES    Client/Worker
(defconstant STATUS_RES 20)  ;;          RES    Client
(defconstant SUBMIT_JOB_HIGH 21)  ;;     REQ    Client
(defconstant SET_CLIENT_ID 22)  ;;       REQ    Worker
(defconstant CAN_DO_TIMEOUT 23)  ;;      REQ    Worker
(defconstant ALL_YOURS 24)  ;;           REQ    Worker
(defconstant WORK_EXCEPTION 25)  ;;      REQ    Worker
                                            ;;RES    Client
(defconstant OPTION_REQ 26)  ;;          REQ    Client/Worker
(defconstant OPTION_RES 27)  ;;          RES    Client/Worker
(defconstant WORK_DATA 28)  ;;           REQ    Worker
                                       ;;RES    Client
(defconstant WORK_WARNING 29)  ;;        REQ    Worker
                                           ;; RES    Client
(defconstant GRAB_JOB_UNIQ 30)  ;;       REQ    Worker
(defconstant JOB_ASSIGN_UNIQ 31)  ;;     RES    Worker
(defconstant SUBMIT_JOB_HIGH_BG 32)  ;;  REQ    Client
(defconstant SUBMIT_JOB_LOW 33)  ;;      REQ    Client
(defconstant SUBMIT_JOB_LOW_BG 34)  ;;   REQ    Client
(defconstant SUBMIT_JOB_SCHED 35)  ;;    REQ    Client
(defconstant SUBMIT_JOB_EPOCH 36)  ;;    REQ    Client

(defparameter +request-magic+ #(0 82 69 81)) ;  \0REQ
(defparameter +response-magic+ #(0 82 69 83)) ; \0RES

(defparameter +name-table+ (let ((h (make-hash-table)))
                            (setf (gethash CAN_DO h) :CAN_DO)
                            (setf (gethash CANT_DO h) :CANT_DO)
                            (setf (gethash RESET_ABILITIES h) :RESET_ABILITIES)
                            (setf (gethash PRE_SLEEP h) :PRE_SLEEP)
                            (setf (gethash NOOP h) :NOOP)
                            (setf (gethash SUBMIT_JOB h) :SUBMIT_JOB)
                            (setf (gethash JOB_CREATED h) :JOB_CREATED)
                            (setf (gethash GRAB_JOB h) :GRAB_JOB)
                            (setf (gethash NO_JOB h) :NO_JOB)
                            (setf (gethash JOB_ASSIGN h) :JOB_ASSIGN)
                            (setf (gethash WORK_STATUS h) :WORK_STATUS)
                            (setf (gethash WORK_COMPLETE h) :WORK_COMPLETE)
                            (setf (gethash WORK_FAIL h) :WORK_FAIL)
                            (setf (gethash GET_STATUS h) :GET_STATUS)
                            (setf (gethash ECHO_REQ h) :ECHO_REQ)
                            (setf (gethash ECHO_RES h) :ECHO_RES)
                            (setf (gethash SUBMIT_JOB_BG h) :SUBMIT_JOB_BG)
                            (setf (gethash ERROR h) :ERROR)
                            (setf (gethash STATUS_RES h) :STATUS_RES)
                            (setf (gethash SUBMIT_JOB_HIGH h) :SUBMIT_JOB_HIGH)
                            (setf (gethash SET_CLIENT_ID h) :SET_CLIENT_ID)
                            (setf (gethash CAN_DO_TIMEOUT h) :CAN_DO_TIMEOUT)
                            (setf (gethash ALL_YOURS h) :ALL_YOURS)
                            (setf (gethash WORK_EXCEPTION h) :WORK_EXCEPTION)
                            (setf (gethash OPTION_REQ h) :OPTION_REQ)
                            (setf (gethash OPTION_RES h) :OPTION_RES)
                            (setf (gethash WORK_DATA h) :WORK_DATA)
                            (setf (gethash WORK_WARNING h) :WORK_WARNING)
                            (setf (gethash GRAB_JOB_UNIQ h) :GRAB_JOB_UNIQ)
                            (setf (gethash JOB_ASSIGN_UNIQ h) :JOB_ASSIGN_UNIQ)
                            (setf (gethash SUBMIT_JOB_HIGH_BG h) :SUBMIT_JOB_HIGH_BG)
                            (setf (gethash SUBMIT_JOB_LOW h) :SUBMIT_JOB_LOW)
                            (setf (gethash SUBMIT_JOB_LOW_BG h) :SUBMIT_JOB_LOW_BG)
                            (setf (gethash SUBMIT_JOB_SCHED h) :SUBMIT_JOB_SCHED)
                            (setf (gethash SUBMIT_JOB_EPOCH h) :SUBMIT_JOB_EPOCH)
                            h))

(defclass message ()
  ((magic
    :initarg  :magic
    :initform +request-magic+
    :reader   magic)
   (name
    :initarg  :name
    :reader   name)
   (data
    :initarg  :data
    :initform nil
    :reader   data)
   ))

(defmethod message-name? ((msg message) n)
  (with-slots (name) msg
    (eql n name)))

(defmethod print-object ((msg message) stream)
  (print-unreadable-object (msg stream :type t)
    (with-slots (magic name data) msg
      (format stream "~[REQ~;RES~] ~a ~a"
              (if (equalp magic +request-magic+) 0 1)
              (gethash name +name-table+)
              data))))

(defun message-to-octets (msg)
  (let ((buf (make-array 128 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8)))
        (buf2 (make-array 128 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8))))

    (with-slots (magic name data) msg
      (loop for i in data
         and notfirst = nil then t
         do (progn
              (if notfirst (vector-push-extend 0 buf2))
              (if (not (null i)) (vector-join nil buf2 (babel:string-to-octets i)))))
      
      (vector-join nil
                   buf
                   magic
                   (uint32-to-big-endian-octets name)
                   (uint32-to-big-endian-octets (length buf2))
                   buf2))
    buf))

(defun split-payloads (payloads)
  (loop for s in (split-sequence:split-sequence 0 payloads) collect (babel:octets-to-string s)))

(defun write-message (stream message)
  (let ((packet (message-to-octets message)))
    (write-sequence packet stream)
    (force-output stream)))

(defun read-message (stream)
  (let* ((magic (read-as-vector 4 stream))
         (name (big-endian-octets-to-uint32 (read-as-vector 4 stream)))
         (siz (big-endian-octets-to-uint32 (read-as-vector 4 stream)))
         (payloads (read-as-vector siz stream :element-type '(unsigned-byte 8))))
    (make-instance 'message
                   :magic magic
                   :name name
                   :data (split-payloads payloads))))

(defclass job ()
  ((handle :initarg :handle)
   (name :initarg :name)
   (arg :initarg :arg)
   (uid :initarg :uid :initform nil)))

(defmethod print-object ((j job) stream)
  (print-unreadable-object (j stream :type t)
    (with-slots (handle name arg) j
      (format stream "~a ~a ~a" handle name arg))))

(defmethod job-assign-message-to-job ((msg message))
  "convert JOB_ASSIGN message to job object."
  (with-slots (data) msg
    (let ((handle (car data))
          (name (cadr data))
          (arg (caddr data)))
      (make-instance 'job :handle handle :name name :arg arg))))

(defmethod job-created-message-to-job ((msg message) &key name arg uid)
  "convert JOB_CREATED message to job object."
  (with-slots (data) msg
    (let ((handle (car data)))
      (make-instance 'job :handle handle :name name :arg arg :uid uid))))
