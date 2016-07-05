(import (scheme base)
	(scheme write)
	(rapid syntax-parameters))

(define-syntax-parameter abort
  (syntax-rules ()
    ((_ . _)
     (syntax-error "abort used outside of a loop"))))

(define-syntax forever
  (syntax-rules ()
    ((forever body1 body2 ...)
     (call-with-current-continuation
      (lambda (escape)
	(syntax-parameterize
	    ((abort
	     (syntax-rules ()
	       ((abort value (... ...))
		(escape value (... ...))))))
	  (let loop ()
 	    body1 body2 ... (loop))))))))

(define i 0)

(display
 (forever
  (display i)
  (newline)
  (set! i (+ 1 i))
  (when (= i 10)
    (abort i))))

(newline)
 
