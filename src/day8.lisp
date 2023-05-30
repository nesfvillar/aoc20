(ql:quickload "cl-ppcre")

(defpackage :aoc20/8
  (:use :cl)
  (:export
   #:part-2
   #:part-2))

(in-package :aoc20/8)

(defconstant input
  (mapcar
   (lambda (line)
     (cl-ppcre:split "\\s" line))
   (uiop:read-file-lines #P"../inputs/day8.txt")))

(defstruct instruction
  :op
  :val)

(defconstant instructions
  (make-array
   (length input)
   :initial-contents
   (loop :for (op val) :in input
	 :collect
	 (make-instruction :op (read-from-string op)
			   :val (parse-integer val)))))

(defstruct emulator
  (acc 0)
  (pc 0))

(defun acc (nemulator value)
  (progn
    (setf (emulator-acc nemulator)
	  (+ (emulator-acc nemulator)
	     value))
    (setf (emulator-pc nemulator)
	  (1+ (emulator-pc nemulator)))))

(defun jmp (nemulator value)
  (setf (emulator-pc nemulator)
	(+ (emulator-pc nemulator)
	   value)))

(defun nop (nemulator _)
  (setf (emulator-pc nemulator)
	(1+ (emulator-pc nemulator))))

(defun emulator-loop (nemulator instruction)
  (progn
    (funcall (instruction-op instruction)
	     nemulator
	     (instruction-val instruction))
    nemulator))

(defun part-1 ()
  (let ((em (make-emulator))
	(seen nil))
    (loop :for pc = (emulator-pc em)
	  :for ins = (aref instructions pc)
	  :unless (member pc seen) :do
	    (progn
	      (emulator-loop em ins)
	      (setf seen (cons pc seen)))
	  :else :do
	    (return (emulator-acc em)))))

(defun part-2 ()
  ())
