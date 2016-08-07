(define-library (rapid error test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid error))
  (begin
    (define (run-tests)
      (test-begin "Error reporting")

      (test-equal "Error prints message to current error port"
		  "./a.out: error\n"
		  (parameterize ((current-error-port (open-output-string)))
		    (rapid-error 0 "~a" "error")
		    (get-output-string (current-error-port))))
      
      (test-end)

      #t)))
