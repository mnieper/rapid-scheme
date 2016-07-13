(import (rapid primitive))

(define-values (+) 'foo)

(define-syntax macro1
  (syntax-rules ()
    ((macro1 op)
     (begin
       (define-syntax macro2
	 (syntax-rules (opa opa)
	   ((macro2 opa) 'foo)))
       (macro2 op)))))

(macro1 +)
