#;(cond-expand
 (chibi
  (import (scheme base)))
 (else
  (import (rapid primitive)))) 

(import (rapid primitive))

(define-values (+) 'foo)

(define-syntax macro1
  (syntax-rules ()
    ((macro1 op)
     (begin
       (define-syntax macro2
	 (syntax-rules (op op)
	   ((macro2 op) 'foo)))
       (macro2 op)))))

(macro1 +)
