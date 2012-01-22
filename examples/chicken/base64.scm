#!/usr/bin/env csi -q
(use (prefix base64 b64:))

(let* [(s "foo blah qux baz")
       (encoded (b64:base64-encode s))]
  (printf "base64 of ~s => ~s => ~s\n" 
          s
          encoded
          (b64:base64-decode encoded)))

(exit)
