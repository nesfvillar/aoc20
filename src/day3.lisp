(defpackage :aoc20/3
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/3)

(defconstant input
   (uiop:read-file-lines #P"../inputs/day3.txt"))

(defconstant input-width
  (length (first input)))

(defconstant tree
  #\#)

(defun count-trees (dx dy &optional (x 0)(forest input) (count 0))
  (if (null forest)
      count
      (let* ((tree-line (first forest))
	     (tile (char tree-line (mod x input-width))))
	(if (char= tile tree)
	    (count-trees dx dy (+ x dx) (nthcdr dy forest) (1+ count))
	    (count-trees dx dy (+ x dx) (nthcdr dy forest) count)))))

(defun part-1 ()
  (count-trees 3 1))

(defconstant slopes
  '((1 1)
    (3 1)
    (5 1)
    (7 1)
    (1 2)))

(defun part-2 ()
  (reduce #'*
	  (mapcar
	   (lambda (slope)
	     (apply #'count-trees slope))
	   slopes)))
