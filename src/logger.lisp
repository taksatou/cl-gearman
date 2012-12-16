#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-gearman)

(defconstant +debug+ 0)
(defconstant +info+ 1)
(defconstant +warn+ 2)
(defconstant +error+ 3)
(defconstant +fatal+ 4)

(defun default-formatter (message &key timestamp level tag)
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time timestamp)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~[DEBUG~;INFO ~;WARN ~;ERROR~;FATAL~] ~a ~a"
            year month date hour minute second
            level
            (if tag tag "")
            message)))

(defparameter *logger-output* *standard-output*)
(defparameter *logger-formatter* #'default-formatter)
(defparameter *logger-level* +info+)

(defun write-log (level tag message)
  (if (>= level *logger-level*)
      (write-line
       (funcall *logger-formatter* message :timestamp (get-universal-time) :level level :tag tag)
       *logger-output*)) nil)

(defun log-debug (message &optional (tag "")) (write-log +debug+ tag message))
(defun log-info  (message &optional (tag "")) (write-log +info+ tag message))
(defun log-warn  (message &optional (tag "")) (write-log +warn+ tag message))
(defun log-error (message &optional (tag "")) (write-log +error+ tag message))
(defun log-fatal (message &optional (tag "")) (write-log +fatal+ tag message))

