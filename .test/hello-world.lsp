(print (+ 1 2))

(princ "ceiling: ")
(write (ceiling 34.34))

(format t "~%Hello, World!~%")
;; (SB-EXT:EXIT)

(defun fib (n)
    "Return the nth Fibonacci number."
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))
    )
)

(write (fib 30))
(terpri)
(terpri)

(loop
    (princ "input: ")
    (force-output)
    (let ((x (read-line)))
        (format t ">> ~2d" x)
        (terpri)
    )
)