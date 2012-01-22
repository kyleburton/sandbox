#!/usr/bin/env csi -q
(use posix)
(use ports)


(define (string->sexp s)
    (call-with-input-string s
                                (lambda (p) 
                                        (read p))))

(let ((input "1"))
  (printf "parsing: ~s yields: ~s\n" input (string->sexp input)))

(exit)

