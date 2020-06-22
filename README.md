# Cl-Gearman

Common Lisp Library for the Gearman distributed job system.

## Installation

cl-gearman is available on quicklisp.

```
(ql:quickload 'cl-gearman)
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

## Example

Run gearman server,

```
docker run --rm -p 4730:4730 artefactual/gearmand:1.1.19.1-alpine
```

and then,

```
# start worker process
sbcl --script examples/worker.lisp

# start client process
sbcl --script examples/client.lisp
```

## Local development with quicklisp

```
ln -s path/to/cl-gearman-repo ~/quicklisp/local-projects/cl-gearman
```


## lisp

- sbcl
- clisp
- clozure cl
- allegro cl

## Author

* Takayuki Sato

## Copyright

Copyright (c) 2012 Takayuki Sato

# License

Licensed under the LLGPL License.

