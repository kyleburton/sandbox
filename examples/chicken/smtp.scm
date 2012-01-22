#!/usr/bin/env csi -q
;;;; smtp.scm
;
; Copyright (c) 2000-2008, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Steinweg 1A
; 37130 Gleichen, OT Weissenborn
; Germany


;;(declare 
 ;;(uses tcp extras regex)
 ;;(export smtp:connect smtp:disconnect smtp:open smtp:smtp?) )
(use tcp extras regex)

(define-constant default-smtp-port 25)

(define-record smtp
  open					; bool
  verbose				; bool
  in					; port
  out)					; port

(define smtp:smtp? smtp?)

(define (smtp:connect host localhost . more)
  (let-optionals more ([verbose #f] [port default-smtp-port])
    (let-values ([(i o) (tcp-connect host port)])
      (let ([smtp (make-smtp #f verbose i o)])
	(fetch smtp)
	(send smtp "HELO ~A" localhost)
	(fetch smtp) 
	smtp) ) ) )

(define (smtp:disconnect smtp)
  (send smtp "QUIT") 
  (fetch smtp)
  (close-input-port (smtp-in smtp))
  (close-output-port (smtp-out smtp)) )

(define (send smtp fstr . args)
  (when (smtp-verbose smtp) (fprintf (current-error-port) "SMTP: ~?~%" fstr args))
  (fprintf (smtp-out smtp) "~?\r\n" fstr args) 
  (flush-output (smtp-out smtp) ) )

(define (smtp-error msg code)
  (signal
   (make-composite-condition
    (make-property-condition 'exn 'message msg)
    (make-property-condition 'smtp 'code code) ) ) )

(define (fetch smtp)
  (let loop ([data '()])
    (let ([ln (read-line (smtp-in smtp))])
      (when (eof-object? ln)
	(smtp-error "unexpected end of SMTP reply" 0) )
      (when (smtp-verbose smtp) (fprintf (current-error-port) "SMTP: [~A]~%" ln))
      (match (string-match "([0-9]{3})(-| )(.*)" ln)
	[(_ code cont msg)
	 (if (string=? cont "-")
	     (loop (cons msg data))
	     (let ([ncode (string->number code)])
	       (when (>= ncode 400)
		 (smtp-error (apply string-append (reverse (cons msg data))) ncode) ) ) ) ]
	[_ (smtp-error "invalid reply from SMTP server" 0)] ) ) ) )

(define (smtp:open smtp from to1 . to)
  (when (smtp-open smtp)
    (smtp-error "SMTP data transfer already in progress" 0) )
  (send smtp "MAIL FROM: <~A>" from)
  (fetch smtp)
  (for-each 
   (lambda (to)
     (send smtp "RCPT TO: <~A>" to)
     (fetch smtp) )
   (cons to1 to) )
  (send smtp "DATA")
  (fetch smtp)
  (let ([bol #t])
    (make-output-port
     (lambda (s)
       (let ([len (string-length s)])
	 (when (fx>= len 1)
	   (when (and bol (char=? #\. (string-ref s 0)))
	     (write-char #\. (smtp-out smtp)))
	   (display
	    (string-translate* s '(("\r\n." . "\r\n..")))
	    (smtp-out smtp))
	   (set! bol (char=? #\newline (string-ref s (fx- len 1)))) ) ) ) 
     (lambda ()
       (unless bol
	 (display "\r\n"))
       (display ".\r\n" (smtp-out smtp))
       (flush-output (smtp-out smtp))
       (fetch smtp)
       (smtp-open-set! smtp #f) ) ) ) )


(use posix)


(define s (smtp:connect "localhost" (get-host-name)))
(define p (smtp:open s "kyle.burton@gmail.com" "kyle.burton@gmail.com"))
(display "Subject: How are you?\r\n\r\nSo, how are you then?\r\n\n" p)
(close-output-port p)
(smtp:disconnect s)
