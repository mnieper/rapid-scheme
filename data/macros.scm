(import (rapid primitive))

(define-syntax foo
  (syntax-rules ()
    ((foo a) 1)))

(foo a)
