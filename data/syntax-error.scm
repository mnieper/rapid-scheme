(import (rapid primitive))

(define-values (f)
  (case-lambda
   ((a) 42)))

(f a)

(if (begin
      a)
    #f)

(define-values x (unknown))
