(import (scheme base) (scheme write) (scheme read))

(define (port->sexp p)
  (define (itr cur)
    (let ((r (read p)))
     (if (eof-object? r)
       (reverse cur)
       (itr (cons r cur))) ))
  (itr '()))

(write (port->sexp (open-input-string "a")))
(newline)
