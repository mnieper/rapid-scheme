(define-library (rapid args-fold test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid args-fold))
  (begin
    (define (option-proc option name arg seed)
      (cons (list name arg) seed))
    (define options
      (list (option '(#\h "help") #f #f option-proc)
	    (option '(#\v "version") #f #f option-proc)
	    (option '(#\o "output") #t #f option-proc)))
    (define (unrecognized-option-proc option name arg seed)
      (cons (list 'unrecognized-option name arg) seed))
    (define (operand-proc operand seed)
      (cons (list 'operand operand) seed))
    (define (scan-args args)
      (reverse
       (args-fold args options unrecognized-option-proc operand-proc '())))

    (define (run-tests)
      (test-begin "A program argument processor")

      (test-equal "Long-option argument as separate argument"
		  '(("output" "test.o"))
		  (scan-args '("--output" "test.o")))

      (test-equal "Long-option argument in a single argument string"
		  '(("output" "test.o"))
		  (scan-args '("--output=test.o")))
      
      (test-equal "Parsing options with and without required arguments"
		  '(("help" #f)
		    (#\h #f)
		    (#\v #f)
		    ("output" "file.o")
		    (operand "file.c"))
		  (scan-args '("--help" "-hv" "--output" "file.o" "file.c")))
 
      (test-end)

      #t)))
