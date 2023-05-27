(ql:quickload "cl-ppcre")

(defpackage :aoc20/2
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/2)

(defconstant input
  (mapcar
   (uiop:read-file-lines #P"../inputs/day2.txt")))


(defun parse-password (string)
  (destructuring-bind
      (formatting password)
      (cl-ppcre:split ":\\s?" string)
    (destructuring-bind
	(range char)
	(cl-ppcre:split "\\s" formatting)
      (destructuring-bind
	  (l-range u-range)
	  (mapcar #'parse-integer
		  (cl-ppcre:split "-" range))
	(list l-range u-range (char char 0) password)))))

(defun part-1 ()
  (count-if
   (lambda (password)
     (destructuring-bind (l-range u-range char pass)
	 (parse-password password)
       (<= l-range (count char pass) u-range)))
   input))

(defun xor (a b)
  (if a (not b) b))

(defun char-at (string index)
  (char string (1- index)))

(defun part-2()
  (count-if
   (lambda (password)
     (destructuring-bind (first second char password)
	 (parse-password password)
       (xor
	(char= char (char-at password first))
	(char= char (char-at password second)))))
   input))
