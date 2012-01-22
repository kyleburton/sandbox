#!/usr/bin/env csi -q
(use smtp)

(define s (smtp:connect "localhost" (get-host-name)))
(define p (smtp:open s "kyle.burton@gmail.com" "kyle.burton@gmail.com"))
(display "Subject: How are you?\r\n\r\nSo, how are you then?\r\n\n" p)
(close-output-port p)
(smtp:disconnect s)
