(import (lib)
        (rapid primitive))

(define-record-type <comparator>
  (%make-comparator type-test equality ordering hash)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function))

%make-comparator


(define-syntax quux
  (syntax-rules (=> if)
    ((quux =>)
     1)
    ((quux . _)
     (syntax-error ":-("))))

(quux =>)

(case-lambda
 (()
  (if #f #f)))

(define-syntax foo
  (syntax-rules ()
    ((foo a) 1)))

(foo a)

(case-lambda
 (a
  (define-values (if) 1)
  if))

(include "bar.scm")

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
