(ql:quickload "cl-ppcre")
(load #P"src/utils.lisp")

(defpackage :aoc20/7
  (:use :cl
	:aoc20/utils)
  (:export
   #:part-1
   #:part-2))
(in-package :aoc20/7)

(defun split-line (line)
  (cl-ppcre:split "\\scontain\\s" line))

(defun split-rules (rules)
  (mapcar
   (lambda (rule)
     (cl-ppcre:regex-replace "\\." rule ""))
   (cl-ppcre:split ",\\s" rules)))

(defun parse-rule (rule)
  (if (string= "no other bags" rule)
      nil
      (cons
       (cl-ppcre:regex-replace-all "\\d\\s+\|\\s*bags?" rule "")
       (parse-integer (first (cl-ppcre:split "\\s" rule))))))


(defun parse-line (string)
  (let* ((line (split-line string))
	 (bag (cl-ppcre:regex-replace "\\s*bags?\\s*" (car line) ""))
	 (rules (mapcar
		 (lambda (rule)
		   (parse-rule rule))
		 (split-rules (cadr line)))))
    (cons bag rules)))

(defconstant input
  (mapcar
   #'parse-line
   (uiop:read-file-lines #P"inputs/day7.txt")))

(defun-memoized can-hold-shiny-p (bag)
  (if (assoc "shiny gold" (cdr bag) :test #'string=) t
      (some (lambda (bag)
	      (can-hold-shiny-p (assoc (car bag) input :test #'string=)))
	    (cdr bag))))

(defun part-1 ()
  (count-if
   #'can-hold-shiny-p
   input))

(defun count-bags-inside (bag)
  (if (equal '(nil) (cdr bag)) 0
      (reduce #'+
	      (mapcar
	       (lambda (bag)
		 (* (1+ (count-bags-inside (assoc (car bag) input :test #'string=))) (cdr bag)))
	      (cdr bag)))))

(defun part-2 ()
   (count-bags-inside
    (assoc "shiny gold" input :test #'string=)))
