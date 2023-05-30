(ql:quickload "cl-ppcre")

(defpackage :aoc20/2
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/2)

(defstruct password-field
  first
  second
  char
  password)

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
	(make-password-field
	 :first l-range
	 :second u-range
	 :char (char char 0)
	 :password password)))))

(defconstant input
  (mapcar
   #'parse-password
   (uiop:read-file-lines #P"../inputs/day2.txt")))

(defun part-1 ()
  (count-if
   (lambda (password)
     (<=
      (password-field-first password)
      (count (password-field-char password)
	     (password-field-password password))
      (password-field-second password)))
   input))

(defun xor (a b)
  (if a (not b) b))

(defun char-at (string index)
  (char string (1- index)))

(defun part-2()
  (count-if
   (lambda (password)
     (xor
      (char= (password-field-char password)
	     (char-at (password-field-password password)
		      (password-field-first password)))
      (char= (password-field-char password)
	     (char-at (password-field-password password)
		      (password-field-second password)))))
   input))
