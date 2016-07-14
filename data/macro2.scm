(import (rapid primitive))

(define-syntax foo
  (syntax-rules (a)
    ((foo a)
     'bar)))

(foo a)
