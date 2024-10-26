(format t "~%Hello, World!~%~%")

(defun input (value)
    "simplified input system"
    (format t "~a " value)
    (force-output)
    (read-line)
)

;; thanks to 'zacque0' at https://acessa.ae/lisp-ctrlc-handler
(handler-bind 
    ((sb-sys:interactive-interrupt
        (lambda (c) (declare (ignore c))
            (format *error-output* "~&~%Interrupted!~%")
            (sb-ext:exit :code 1 :abort t)
        )
    ))
    ;;
    (loop
        (let ((x (input ">")))
            (if (string= x "ext")
                (sb-ext:exit :code 1 :abort t)
                (format t "output: ~2d" x)
            )
        )
        (terpri)
    )
)

;; Browser functions 

;; (get-element "id")                       // get element from ID
;; (get-elements "class")                   // get list of elements from class
;; (popup -a "this is an alert!")           // `-a`: 'alert'
;; (poupup -c "do you want to proceed?")    // `-c`: 'confirirmation', can be stored as a boolean value
;; (popup -p "password:")                   // `-p`: 'promp', can be stored as a string