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

(defun input (value)
    "input system"
    (format t "~a " value)
    (force-output)
    (read-line)
)

;; thanks to 'zacque0' at https://arc.net/l/quote/xwoospzk
(handler-bind 
    ((sb-sys:interactive-interrupt
        (lambda (c) (declare (ignore c))
            (format *error-output* "~&Interrupted!~%")
            (sb-ext:exit :code 1 :abort t)
        )
    ))
    ;;
    (format t "Terminal Started!~%")
    ;;
    (force-output)
    (loop
        (let ((x (input ">")))
            (format t "output: ~2d" x)
        )
        (terpri)
    )
)

;; Browser functions 

;; (getElement "id")                        // get element from ID
;; (getElements "class")                    // get list of elements from class
;; (popup -a "this is an alert!")           // `-a`: 'alert'
;; (poupup -c "do you want to proceed?")    // `-c`: 'confirirmation', can be stored as a boolean value
;; (popup -p "password:")                   // `-p`: 'promp', can be stored as a string