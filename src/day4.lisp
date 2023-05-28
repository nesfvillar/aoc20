(ql:quickload "cl-ppcre")

(defpackage :aoc20/4
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/4)

(defconstant input
  (cl-ppcre:split "\\n\\s?\\n" (uiop:read-file-string #P"../inputs/day4.txt")))

(defun credential-parse (credential)
  (mapcar
   (lambda (field)
     (destructuring-bind (key value)
	 (cl-ppcre:split ":" field)
       (cons (read-from-string key) value)))
   (cl-ppcre:split "\\s+" credential)))

(defconstant neccesary-fields
  '(byr
    iyr
    eyr
    hgt
    hcl
    ecl
    pid))

(defun part-1 ()
  (count-if
   (lambda (credential)
     (notany
      (lambda (field)
	(null (assoc field credential)))
      neccesary-fields))
   (mapcar
    #'credential-parse
    input)))

(defun part-2 ())
