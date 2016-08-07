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

      (test-equal "append-reverse"
		  '(1 2 . 3)
		  (append-reverse '(2 1) 3))
      
      (test-equal "fold-right"
		  '(2 2 4 4 6)
		  (fold-right (lambda (x l)
				(if (even? x) (cons x l) l))
			      '() '(1 2 2 3 4 4 5 5 6)))

      (test-assert "every - unequal length"
		   (every = (list 1 2) (list 1)))

      (test-assert "every - equal length"
		   (not (every = (list 1 2) (list 1 3))))

      (test-end)

      #t)))
