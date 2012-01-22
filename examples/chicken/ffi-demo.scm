;; see also:
;;    http://blog.alwaysmovefast.com/using-epoll-with-chicken-scheme
(module 
  ffi-demo
  ()

  (import chicken scheme foreign)
  (use srfi-69)
  (use srfi-4)

  (foreign-declare #<<END
#include <stdio.h>
END
)

  (define my-strlen
    (foreign-lambda int "strlen" c-string))

  (printf "strlen: ~A\n" (my-strlen "foof"))
  (foreign-code "printf(\"beep, from c's printf!\\n\");"))
