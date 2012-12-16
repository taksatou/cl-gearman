#|
  This file is a part of cl-gearman project.
  Copyright (c) 2012 Takayuki Sato
|#

(in-package :cl-gearman)

(defparameter *uid-random-state* (make-random-state t))

(defun vector-join (sep &rest vectors)
  "Join vectors. The first vector must be adjustable or contain enough space to append following vectors."
  (let ((v (car vectors)))
    (loop for x in (cdr vectors)
       and isfirst = t then nil do (progn
                                     (cond ((null sep))
                                           (isfirst ())
                                           ((vectorp sep) (loop for y across sep do (vector-push-extend y v)))
                                           ((listp sep) (loop for y in sep do (vector-push-extend y v)))
                                           (t (vector-push-extend sep v)))
                                     (loop for y across x do (vector-push-extend y v))))
    v))

(defun uint32-to-big-endian-octets (num)
  (let ((res (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for (x y) = (multiple-value-list (floor num 256)) then (multiple-value-list (floor x 256))
       and pos = 3 then (1- pos)
       until (and (zerop x) (zerop y))
       do (setf (elt res pos) y))
    res))

(defun big-endian-octets-to-uint32 (octets)
  (loop for b across octets
     and pos = 3 then (1- pos)
     sum (* b (expt 2 (* 8 pos)))))

(defun make-uid ()
  (format nil "~{~2,'0x~}" (loop for i upto 15 collect (random 256 *uid-random-state*))))
;;(loop for c across (secure-random:bytes 16 secure-random:*generator*) collect c)))

(defun list-to-vector (lis)
  (let ((vec (make-array 8 :fill-pointer 0 :adjustable t)))
    (loop for x in lis do (vector-push-extend x vec))
    vec))

(defun read-as-vector (siz stream &key (element-type t))
  (let ((vec (make-array 4 :fill-pointer 0 :adjustable t :element-type element-type)))
    (loop repeat siz do (vector-push-extend (read-byte stream) vec))
    vec))

