(import (scheme base)
        (scheme write))

(define-syntax match-check-ellipse
  (syntax-rules ()
    ;; these two aren't necessary but provide fast-case failures
    ((match-check-ellipse (a . b) success-k failure-k) failure-k)
    ((match-check-ellipse #(a ...) success-k failure-k) failure-k)
    ;; matching an atom
    ((match-check-ellipse id success-k failure-k)
     (let-syntax ((ellipse? (syntax-rules ()
                              ;; iff `id' is `...' here then this will
                              ;; match a list of any length
                              ((ellipse? (foo id) sk fk) sk)
                              ((ellipse? other sk fk) fk))))
       ;; this list of three elements will only many the (foo id) list
       ;; above if `id' is `...'
       (ellipse? (a b c) success-k failure-k)))))

(display (match-check-ellipse ... "OK " "wrong\n"))
(display (match-check-ellipse bogus "wrong\n" "OK\n"))
