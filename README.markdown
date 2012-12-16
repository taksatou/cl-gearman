# Cl-Gearman

Common Lisp Library for the Gearman distributed job system.

## Installation

```
cd ~/quicklisp/local-projects
git clone https://github.com/taksatou/cl-gearman.git
```

## Usage

```
;; client
(cl-gearman:with-client (client "localhost:4730")
  (format t "~a~%" (cl-gearman:submit-job client "echo" :arg "foo")))


;; worker

(cl-gearman:with-worker (worker "localhost:4730") 
  (cl-gearman:add-ability worker "echo"
                          #'(lambda (arg job) arg))
  (loop do (cl-gearman:work worker)))

```

Please see examples for detail.

## lisp

- sbcl
- clisp
- clozure cl

## Author

* Takayuki Sato

## Copyright

Copyright (c) 2012 Takayuki Sato

# License

Licensed under the LLGPL License.

