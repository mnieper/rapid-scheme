(import (rapid primitive))

(define-syntax foo
  (syntax-rules ()
    ((foo a) 1)))

(foo a)

(define-syntax bar
  (syntax-rules ()
    ((bar a b ...)
     ((a b) ...))))

(bar 1 2 3 4)

(define-syntax define
  (syntax-rules ()
    ((define (variable . formals) body1 body2 ...)
     (define-values (variable)
       (lambda formals body1 body2 ...)))
    ((define variable expression)
     (define-values (variable) expression))
    ((define . args)
     (syntax-error "bad define syntax"))))

(define a 2)
