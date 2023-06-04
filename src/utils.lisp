(defpackage :aoc20/utils
  (:use :cl)
  (:export #:defun-memoized))

(in-package :aoc20/utils)

(defmacro defun-memoized (name args &body body)
  (let ((map (gensym)))
    `(let ((,map (make-hash-table)))
       (defun ,name ,args
	 (if (gethash (list ,@args) ,map)
	     (gethash (list ,@args) ,map)
	     (setf (gethash (list ,@args) ,map)
		   ,@body))))))
