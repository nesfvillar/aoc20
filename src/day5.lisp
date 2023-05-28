(defpackage :aoc20/5
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/5)

(defconstant input
  (uiop:read-file-lines #P"../inputs/day5.txt"))

(defun parse-char (char)
  (cond
    ((or (char= #\F char) (char= #\L char)) 0)
    ((or (char= #\B char) (char= #\R char)) 1)
    (t nil)))

(defun parse-longitudinal (string)
  (loop :for i :from 0 :to 6
	:collect (* (expt 2 (- 6 i))
		    (parse-char (char string i)))))

(defun parse-transversal (string)
    (loop :for i :from 0 :to 2
	:collect (* (expt 2 (- 2 i))
		    (parse-char (char string (+ i 7))))))

(defun parse-line (string)
  (destructuring-bind (row col)
      (list (reduce #'+ (parse-longitudinal string))
	    (reduce #'+ (parse-transversal string)))
    (cons row col)))

(defun get-seat-id (location)
  (+ (* (car location) 8)
     (cdr location)))

(defun part-1 ()
  (apply #'max
	 (mapcar
	  (lambda (line)
	    (get-seat-id (parse-line line)))
	  input)))

(defun part-2 ()
  ())
