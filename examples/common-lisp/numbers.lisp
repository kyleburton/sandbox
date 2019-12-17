(format t "long-float-digits:~a~&" (long-float-digits))
(format t "pi: ~a~&" pi)
(format t "sin(1 1/4 * pi) = ~a~&" (sin (* pi 1.25L0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (long-float-digits) 1024)
(format t "long-float-digits:~a~&" (long-float-digits))
(format t "pi: ~a~&" pi)

(format t "sin(1 1/4 * pi) = ~a~&" (sin (* pi 1.25L0)))
