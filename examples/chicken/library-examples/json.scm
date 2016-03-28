#!/usr/bin/env csi -q
(use medea)

(define some-json #<<END
{
  "field1": "value1",
  "field2": [
    {"a": 1, "b": 2},
    "stuff"
  ]
}
END
  )

(printf "the json: ~a\n" some-json);

(printf "read-json: ~a\n" (read-json some-json))

(define some-data-structure 
    `((time . ,(current-seconds))
      (numbers . #(1 2 3 4))))

(define-syntax with-output-to-string*
  (syntax-rules ()
    ((with-output-to-string* body ...)
     (with-output-to-string
       (lambda ()
         body ...)))))
    

(printf "some-data-structure ~a\n" some-data-structure)
(printf "write ~a\n" 
        (with-output-to-string* (write-json some-data-structure)))

(exit)
