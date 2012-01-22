(use data-structures)

(define strlen
  (foreign-lambda int "strlen" c-string))

(printf "strlen: ~A\n" (strlen "foof"))
