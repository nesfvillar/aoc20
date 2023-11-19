(defpackage :aoc20/5
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/5)

(defun parse-char (char)
  (cond
    ((or (char= #\F char) (char= #\L char)) 0)
    ((or (char= #\B char) (char= #\R char)) 1)
    (t nil)))

(defun parse-row (string)
  (loop :for i :from 0 :to 6
	:collect (* (expt 2 (- 6 i))
		    (parse-char (char string i)))))

(defun parse-col (string)
    (loop :for i :from 0 :to 2
	:collect (* (expt 2 (- 2 i))
		    (parse-char (char string (+ i 7))))))

(defun parse-line (string)
  (cons (reduce #'+ (parse-row string))
	(reduce #'+ (parse-col string))))

(defun get-seat-id (location)
  (+ (* (car location) 8)
     (cdr location)))

(defconstant input
  (mapcar
   #'parse-line
   (uiop:read-file-lines #P"inputs/day5.txt")))

(defun part-1 ()
  (apply #'max
	 (mapcar
	  #'get-seat-id
	  input)))

(defun is-front-or-back-p (location)
  (or (eql #b1111111 (car location))
      (eql 0 (car location))))

(defun part-2 ()
  (let* ((occupied-seats
	  (mapcar
	   #'get-seat-id
	   (remove-if #'is-front-or-back-p
		      input)))
	(min-id (apply #'min occupied-seats))
	(max-id (apply #'max occupied-seats)))
    (loop :for id :from min-id :to max-id
	  :when (null (member id occupied-seats)) :do
	    (return id))))
