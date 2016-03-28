#!/usr/bin/env csi -q
;; See: http://wiki.call-cc.org/eggref/4/spiffy
(use (prefix spiffy sp:) 
     srfi-18  ;; threading
     srfi-19) ;; time

(sp:server-port 8123)
(sp:root-path "./www")

(define (datestamp)
  (string-join
    (list
      (date->string (current-date) "~4")
      (sprintf "~a" (current-milliseconds)))
    "."))

(define (handle-404 path)
  (printf "in handle-404, current-request is: ~a\n" (sp:current-request))
  (printf "in handle-404, current-response is: ~a\n" (sp:current-response))
  (let [(msg (sprintf #<<END
404: Not Found at ~a
The path requested is: ~a

END
(datestamp)
path))]
    (print msg)
   ;; NB: send-status is a wrapped version of send-response for common
   ;; status codes, which wraps your the message with default html
   ;;   (send-status 404 "Not Found" msg)
   (sp:send-response
     #:code 404
     #:reason "Not Found"
     #:body msg
     #:headers `((content-type "text/plain")
                 (content-length ,(string-length msg)))))) 

(sp:handle-not-found handle-404)


;;(define server (start-server))
(define server #f)

(define (start-server)
  (set! server (make-thread 
                 (lambda ()
                   (sp:start-server))))
  (thread-start! server))

(define (stop-server)
  (thread-terminate! server))

(start-server)
(printf "server: ~a\n" server)
(printf "to exit / stop type (exit) and press enter\n")

;; (exit)
