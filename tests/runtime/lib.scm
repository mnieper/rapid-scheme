(define-syntax define
  (syntax-rules ()
    ((define f e ...)
     (begin
       (define-values (f) e ...)))))

(define-record-type <record>
  (make-record bar)
  bar?
  (bar bar set-bar!))

(define foo 1)
