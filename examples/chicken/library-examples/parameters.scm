#!/usr/bin/env csi -q

(define my-switch (make-parameter default:))

(printf "my-switch: ~a\n" (my-switch))
(my-switch 137)
(printf "my-switch: ~a\n" (my-switch))
(my-switch "foof")
(printf "my-switch: ~a\n" (my-switch))

(exit)
