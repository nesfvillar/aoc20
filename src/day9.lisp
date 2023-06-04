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

(defun range (array min-idx max-idx)
  (loop :for i :from min-idx :upto (1- max-idx)
	:collect (aref array i)))

(defun-memoized find-set (sum-val min-idx max-idx)
  (progn
    (if (>= min-idx max-idx)
	(return-from find-set nil))

    (if (apply #'sum-of-p
	       (cons sum-val (range input min-idx max-idx)))
	(return-from find-set (cons min-idx max-idx)))

    (let ((left-result (find-set sum-val
				 (1+ min-idx)
				 max-idx)))
      (if left-result
	  (return-from find-set left-result)))

    (let ((right-result (find-set sum-val
				  min-idx
				  (1- max-idx))))
      (if right-result
	  (return-from find-set right-result)))))


(defun part-2 ()
  (let* ((sum-index (loop :for i :from 25
			  :when (null (find-two-sum i))
			    :do (return i)))
	 (left-result (find-set (aref input sum-index)
				0
				(1- sum-index)))
	 (right-result (find-set (aref input sum-index)
				 (1+ sum-index)
				 (length input))))
    (if left-result
	(+ (car left-result)
	   (cdr left-result))
	(+ (car right-result)
	   (cdr right-result)))))
