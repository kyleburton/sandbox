#!/usr/bin/env csi -q
(use extras)

(define (file:read-lines f)
  (call-with-input-file f (lambda (p) (read-lines p))))

(let* [(fname (cadr (command-line-arguments)))
      (lines (file:read-lines fname))]
  (printf "fname is: ~a\n" fname)
  (printf "contents of file[~s/~s lines] =>\n"
          fname
          (length lines))
  (for-each 
    (lambda (l)
      (printf "line: '~a'\n" l))
    lines))

(exit)
