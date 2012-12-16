(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload 'cl-gearman)
(in-package :cl-gearman)
(setf *logger-level* +debug+)
(defparameter *client* (make-client '("localhost")))
(defparameter *worker* (make-worker '("localhost")))

(add-ability "foo" *worker* #'(lambda (x &key worker job) (format t "this is foo: ~A" (list x worker job))))
(add-ability "bar" *worker* #'(lambda (x &key worker job) (format t "this is bar: ~A" x)))
;; (add-task *client* (make-instance 'task :func "foo"))
;; (add-task *client* (make-instance 'task :func "bar"))
;; (loop do (work *worker*))
