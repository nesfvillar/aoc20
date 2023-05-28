(ql:quickload "cl-ppcre")

(defpackage :aoc20/4
  (:use :cl)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/4)

(defun credential-parse (credential)
  (mapcar
   (lambda (field)
     (destructuring-bind (key value)
	 (cl-ppcre:split ":" field)
       (cons (read-from-string key) value)))
   (cl-ppcre:split "\\s+" credential)))

(defconstant input
  (mapcar
   #'credential-parse
  (cl-ppcre:split "\\n\\s?\\n" (uiop:read-file-string #P"../inputs/day4.txt"))))

(defconstant neccesary-fields
  '(byr
    iyr
    eyr
    hgt
    hcl
    ecl
    pid))

(defun fields-present-p (credential)
  (notany
   (lambda (field)
     (null (assoc field credential)))
   neccesary-fields))
  

(defun part-1 ()
  (count-if
   #'fields-present-p
    input))

(defun field-valid-function (key)
  (cond
    ((eq key 'byr) 'byr-valid-p)
    ((eq key 'iyr) 'iyr-valid-p)
    ((eq key 'eyr) 'eyr-valid-p)
    ((eq key 'hgt) 'hgt-valid-p)
    ((eq key 'hcl) 'hcl-valid-p)
    ((eq key 'ecl) 'ecl-valid-p)
    ((eq key 'pid) 'pid-valid-p)
    ((eq key 'cid) (lambda (_) t))
    (t (lambda (_) nil))))

(defun byr-valid-p (value)
  (<= 1920 (parse-integer value) 2002))

(defun iyr-valid-p (value)
  (<= 2010 (parse-integer value) 2020))

(defun eyr-valid-p (value)
  (<= 2020 (parse-integer value) 2030))

(defun hgt-valid-p (value)
  (cond
    ((not (null (cl-ppcre:scan "\\d*cm" value)))
	 (<= 150 (parse-integer value :junk-allowed t) 193))
    ((not (null (cl-ppcre:scan "\\d*in" value)))
	 (<= 59 (parse-integer value :junk-allowed t) 76))
    (t nil)))

(defun hcl-valid-p (value)
  (not (null (cl-ppcre:scan "#[A-f\\d]{6}" value))))

(defun ecl-valid-p (value)
  (not (null (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))))

(defun pid-valid-p (value)
  (eql 9 (length value)))

(defun part-2 ()
  (count-if
   (lambda (credential)
     (and
      (fields-present-p credential)
      (every
       (lambda (field)
	 (funcall (field-valid-function (car field)) (cdr field)))
       credential)))
    input))
