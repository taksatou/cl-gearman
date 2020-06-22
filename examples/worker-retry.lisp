(ql:quickload :cl-gearman)

(cl-gearman:with-worker (worker "localhost:4730") 
  (cl-gearman:add-ability worker "error"
                          #'(lambda (arg job) (error "something wrong")))

  (loop do (handler-bind ((error #'(lambda (c) (invoke-restart 'cl-gearman:skip-job))))
             (cl-gearman:work worker))))
