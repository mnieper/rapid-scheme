(import (scheme base) 
        (scheme write)) 

(define-syntax bar 
  (syntax-rules () 
    ((bar id1 id2) 
     (let () 
       (define-syntax foo 
         (syntax-rules (id1) 
           ((foo x id1)
            (define x 'match)) 
           ((foo x _) 
            (define x 'fail))))
       (foo id2 id2) 
       id2)))) 

(define-syntax quux 
  (syntax-rules () 
    ((quux x) 
     (bar x id)))) 

(display (quux id)) 
(newline) 
