(defpackage :aoc20/9
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/9)

(defconstant input
  (let ((lines (uiop:read-file-lines #P"../inputs/day9.txt")))
    (make-array (length lines)
		:initial-contents (mapcar
				   #'parse-integer
				   lines))))

(defconstant preamble-size 25)

(defun sum-of-p (val &rest rest)
  (= val
     (reduce #'+ rest)))

(defun find-two-sum (index)
  (let ((val-index (aref input index)))
    (loop :for i :from (1- index)
	    :downto (- index preamble-size)
	  :for val-i = (aref input i)
	  :do (loop :for j :from (1- index)
		      :downto (- index preamble-size)
		    :for val-j = (aref input j)
		    :when (and (not (= i j))
			       (sum-of-p val-index val-i val-j))
			:do
			   (return-from find-two-sum (cons i j))))))

(defun part-1 ()
  (loop :for i :from 25
	:when (null (find-two-sum i))
	  :do (return (aref input i))))

(defun part-2 ()
  ())
