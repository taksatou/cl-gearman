;; Run two gearman servers using
;; docker run --rm -p 4730:4730 artefactual/gearmand:1.1.19.1-alpine
;; docker run --rm -p 4731:4730 artefactual/gearmand:1.1.19.1-alpine

;; 1) Test with two servers
;; 2) Shutdown the first server and check that still got the answer to the client side

(ql:quickload :cl-gearman)

(defparameter *servers* '("localhost:4730" "localhost:4731"))

;; worker side

(cl-gearman:with-multiple-servers-worker (wx *servers*)

  (defvar lisp-info (lambda (arg job)
                      (declare (ignorable arg job))
                      (format nil "~A ~A ~%"
                              (lisp-implementation-type)
                              (lisp-implementation-version))))

  (defvar reverse! (lambda (arg job)
                     (declare (ignorable arg job))
                     (format nil "~a~%" (reverse arg))))

  (cl-gearman:add-ability wx "task1" lisp-info)
  (cl-gearman:add-ability wx "task2" reverse!)

  (loop do (handler-bind ((error #'(lambda (c) (invoke-restart 'cl-gearman:skip-job))))
             (cl-gearman:work wx))))


;; client side

(cl-gearman:with-multiple-servers-client (client *servers*)
  (format t "~A"
          (cl-gearman:submit-job client "task1"
                                 :arg ""))
    (format t "~A"
          (cl-gearman:submit-job client "task2"
                                 :arg "Hello Lisp World2!")))
