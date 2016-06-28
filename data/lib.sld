(define-library (lib)
  (export => foo define)
  (import (rapid primitive))
  (begin
    (define-syntax =>
      (syntax-rules ()
	((=> . _) (syntax-error "invalid syntax"))))


    (define-syntax define
      (syntax-rules ()
	((define f e ...)
	 (begin
	   (define-values (f) e ...)))))

    
    (define-syntax foo
      (syntax-rules ()
	((foo a x)
	 (define a (case-lambda (() x))))))))
    
