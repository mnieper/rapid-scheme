(define-syntax bar
  (syntax-rules ()
    ((bar a b ...)
     ((a b) ...))))
