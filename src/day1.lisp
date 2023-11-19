(defpackage :aoc20/1
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/1)

(defconstant input
  (mapcar #'parse-integer (uiop:read-file-lines #P"inputs/day1.txt")))

(defun sum-to-p (sum sequence)
  (eql sum (reduce #'+ sequence)))

(defun part-1 ()
  (loop :for (m . rest) :on input :do
    (loop :for n :in rest
	  :when (sum-to-p 2020 (list m n)) :do
	    (return-from part-1 (* m n)))))

(defun part-2 ()
  (loop :for (m . rest) :on input :do
    (loop :for (n . rest) :on rest :do
      (loop :for o in rest
	    :when (sum-to-p 2020 (list m n o)) :do
	      (return-from part-2 (* m n o))))))
