#!/usr/bin/env csi -q
(use matchable)

(printf "res: ~s\n"
        (match '()
               [() "empty-list"]
               [(x . y) (cons x y)]))

(printf "res: ~s\n"
        (match '(1)
               [() "empty-list"]
               [(x . y) (cons x y)]))

(printf "res: ~s\n"
        (match '(1 2 3)
               [() "empty-list"]
               [(x . y) (cons x y)]))

(printf "res: ~s\n"
        (match '(1 2 3)
               [() #f]
               [(x . y) (list 'x x 'y y)]))

(printf "res: ~s\n"
        (match '((a . 1) (b . 2))
               [() #f]
               [((x . y) . more) (list 'x x 'y y 'more more)]))

(exit)
