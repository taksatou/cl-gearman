(cl-gearman:with-client (client "localhost:4730")
  ;; simple example
  (format t "~a~%" (cl-gearman:submit-job client "hello"))
  
  ;; job with an argument
  (format t "~a~%" (cl-gearman:submit-job client "echo" :arg "foo")))

(cl-gearman:with-client (client "localhost:4730")
  ;; background job  
  (let* ((job1 (cl-gearman:submit-background-job client "sleep" :arg "1"))
         (job2 (cl-gearman:submit-background-job client "sleep" :arg "1" :priority :low))
         (job3 (cl-gearman:submit-background-job client "sleep" :arg "1" :priority :high))
         (jobs `(:medium ,job1 :low ,job2 :high ,job3)))


    (dotimes (i 5)
      (loop for (k v) on jobs by #'cddr
         do (format t "~a: ~a~%" k (cl-gearman:get-job-status client v)))
      (sleep 1))))
              
