#!/usr/bin/env csi -q
;; see: 
;;   strings: http://api.call-cc.org/doc/srfi-13
;;   dates:   http://api.call-cc.org/doc/srfi-19
;;            http://srfi.schemers.org/srfi-19/srfi-19.html (date formatting directives)
;;
(use 
  srfi-13 ;; strings
  srfi-19);; time

(printf "current seconds: ~a\n" (current-seconds))
(printf "current date ~a\n" (current-date))

(define today (current-date))

(define (pad s)
  (string-pad s 30))

(define (iso-8601-tz-date)
  (date->string (current-date) "~4"))

(printf "~a  ISO-8601 year-month-day format\n"                             (pad (date->string today "~1")))
(printf "~a  ISO-8601 hour-minute-second-timezone format\n"                (pad (date->string today "~2")))
(printf "~a  ISO-8601 hour-minute-second format\n"                         (pad (date->string today "~3")))
(printf "~a  ISO-8601 year-month-day-hour-minute-second-timezone format\n" (pad (iso-8601-tz-date)))
(printf "~a  ISO-8601 year-month-day-hour-minute-second format\n"          (pad (date->string today "~5")))


(exit)
