#!/usr/bin/env csi -q
(use matchable)
(use posix)

(printf "Your home is: ~s\n" (get-environment-variable "HOME"))

(match-let 
  loop
  ([([var . val] . rest) (get-environment-variables)])
  (printf "~a=~a\n" var val)
  (if (not (null? rest))
    (loop rest)))

(exit)
