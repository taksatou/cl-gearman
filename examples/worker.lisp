(ql:quickload :cl-gearman)

(cl-gearman:with-worker (worker "localhost:4730") 
  (cl-gearman:add-ability worker "hello"
                          #'(lambda (arg job) "hello"))

  (cl-gearman:add-ability worker "echo"
                          #'(lambda (arg job) arg))

  (cl-gearman:add-ability worker "sleep"
                          #'(lambda (arg job)
                              (sleep (parse-integer arg))
                              (format nil "job:~A finished~%" job)))

  (loop do (cl-gearman:work worker)))
