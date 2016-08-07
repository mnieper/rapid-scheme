(import (rapid primitive))

(define-values (f)
  (case-lambda
   ((a) 42)))

(f a)

(if (begin
      a)
    #f)

(define-values x (unknown))

(x)

(case-lambda
 (()
  ('foo (unknown))
  ('bla)))

(define-primitive z 'z)

(case-lambda
 (()
  (baz b . x)))
