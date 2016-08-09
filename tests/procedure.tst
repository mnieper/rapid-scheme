(import (rapid primitive))

(define-primitive display 'display)
(define-primitive newline 'newline)
(define-primitive = '=)
(define-primitive - '-)
(define-primitive * '*)

(define-values (f)
  (case-lambda
   ((x) (if (= x 0)
	    1
	    (* x (f (- x 1)))))))
	       
(display (f 3))
(newline)

;; Local Variables:
;; mode: scheme
;; End:
