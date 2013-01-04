#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-user)
(defpackage cl-gearman-asd
  (:use :cl :asdf))
(in-package :cl-gearman-asd)

(defsystem cl-gearman
  :version "0.0.1"
  :author "Takayuki Sato"
  :license "LLGPL"
  :depends-on (:usocket :split-sequence :babel :alexandria)
  :components ((:module src
                        :serial t
                        :components
                        ((:file "package")
                         (:file "misc")
                         (:file "logger")
                         (:file "connection")
                         (:file "protocol")
                         (:file "client")
                         (:file "worker"))))
  
  :description "Common Lisp Library for the Gearman distributed job system."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-gearman-test))))
