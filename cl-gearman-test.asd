#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-user)
(defpackage cl-gearman-test-asd
  (:use :cl :asdf))
(in-package :cl-gearman-test-asd)

(defsystem cl-gearman-test
  :author "Takayuki Sato"
  :license "LLGPL"
  :depends-on (:cl-gearman
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-gearman"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
