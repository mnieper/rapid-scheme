(define-library (rapid lists test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid lists))
  (begin
    (define (run-tests)
    
      (test-begin "Lists")

      (test-eqv "Find first element in list that satisfied given predicate"
		3
		(find (lambda (x)
			(> x 2))
		      '(1 2 3 4)))
      
      (test-end)

      #t)))
