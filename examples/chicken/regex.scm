#!/usr/bin/env csi -q
(use (prefix regex rx:))

(define (trim s)
  (rx:string-substitute "(?:^\\s+|\\s+$)" "" s))

(define (string->lines s)
  (rx:string-split-fields "\\n" (trim s) #:infix))


(exit)
