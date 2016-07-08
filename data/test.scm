(import (rapid primitive))

(define-primitive p 'p)

(define-values (f)
  (case-lambda
   ((a b)
    (p a b))))

(f 1 2)
