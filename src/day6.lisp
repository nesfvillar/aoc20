(defpackage :aoc20/6
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/6)

(ql:quickload "cl-ppcre")

(defconstant input
  (mapcar
   (lambda (group)
     (cl-ppcre:split "\\s+" group))
  (cl-ppcre:split "\\n\\s*\\n"
		  (uiop:read-file-string #P"inputs/day6.txt"))))

(defun strings-to-group (strings)
  (mapcar (lambda (string)
	    (coerce string 'list))
	  strings))

(defconstant parsed-input
  (mapcar
   #'strings-to-group
   input))

(defun group-union (groups &optional (result ()))
  (if
   (null groups) result
   (string-union (cdr groups)
		 (union result
			(car groups)))))

(defun part-1 ()
  (reduce #'+
	  (mapcar
	   (lambda (group)
	     (length (group-union group)))
	   parsed-input)))

(defun char-in-group (char group)
  (some
   (lambda (person)
     (member char person))
   group))

(defun group-intersection (groups &optional (result ()))
  (cond
    ((null groups) result)
    ((null result) (group-intersection (cdr groups)
				       (remove-if-not (lambda (char)
							(char-in-group char (cdr groups)))
						      (car groups))))
    (t (group-intersection (cdr groups)
			   (intersection result
					 (car groups))))))

(defun part-2 ()
  (reduce #'+
	  (mapcar
	   (lambda (group)
	     (length (group-intersection group)))
	   parsed-input)))
