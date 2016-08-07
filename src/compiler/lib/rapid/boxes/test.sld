(define-library (rapid boxes test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid boxes))
  (begin
    (define (run-tests)
    
      (test-begin "Boxes")
      
      (test-assert "Boxing a value yields a box"
		   (box? (box 1)))
      
      (test-eqv "Unboxing retrieves the boxed value"
		(unbox (box 2))
		2)
      
      (test-eqv "Mutating changes the boxed value"
		(let ((b (box 3)))
		  (set-box! b 4)
		  (unbox b))
		4)
      
      (test-end)

      #t)))
