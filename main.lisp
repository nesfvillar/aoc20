(loop for d from 1 to 9 do
    (let ((path (pathname (format nil "src/day~D.lisp" d))))
        (load path)))
