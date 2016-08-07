(import (scheme base)
        (scheme write))

(define-syntax check
  (syntax-rules (lit)
    ((_ lit sk fk)
     sk)
    ((_ others sk fk)
     fk)))

(define lit2 0) 

(define-syntax check2
  (syntax-rules (lit2)
    ((_ lit2 sk fk)
     sk)
    ((_ others sk fk)
     fk)))

(display (check lit "+" "-"))
(display (check bogus "-" "+"))
(display (check2 lit2 "+" "-"))
(display (check2 bogus "-" "+"))
(newline)
