#!/usr/bin/env csi -q
(use spiffy)

(server-port 8123)
(root-path "./www")
(define server (start-server))

(printf "server: ~a\n" server)

(exit)
