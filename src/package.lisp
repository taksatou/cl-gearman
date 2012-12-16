#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-user)
(defpackage cl-gearman
  (:use :cl)
  (:export

   ;; client
   #:client
   #:make-client
   #:submit-job
   #:submit-background-job
   #:get-job-status
   #:close-client
   #:with-client

   ;; worker
   #:worker
   #:make-worker
   #:add-ability
   #:remove-ability
   #:reset-abilities
   #:work
   #:close-worker
   #:with-worker
   #:skip-job
   #:abort-job
   #:retry-job


   ;; misc
   #:*logger-output*
   #:*logger-formatter*
   #:*logger-level*
   #:log-debug
   #:log-info
   #:log-warn
   #:log-error
   #:log-fatal
   
   ))
