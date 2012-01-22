#!/usr/bin/env csi -q
(use (prefix shell sh:)
     (prefix regex rx:))

(define (trim s)
  (rx:string-substitute "(?:^\\s+|\\s+$)" "" s))

(define (string->lines s)
  (rx:string-split-fields "\\n" (trim s) #:infix))

(let [(lines (string->lines (sh:capture (ls))))]
  (printf "output of ls[~a]: ~a\n" (length lines) lines))

(exit)
