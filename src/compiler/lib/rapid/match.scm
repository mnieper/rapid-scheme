;;; Rapid Scheme --- An implementation of R7RS

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-syntax ->
  (syntax-rules ()
    ((-> . _) (syntax-error "invalid use of auxiliary syntax" ->))))   

(define-syntax match
  (syntax-rules ()
    ((match expr (pattern . body) ...)
     (match-aux expr ((pattern . body) ...)))))

(define-syntax match-aux
  (syntax-rules (unquote -> guard)

    ((match-aux e clause*)
     (let loop ((expr e))
       (call-with-current-continuation
	(lambda (return)
	  (match-aux "match" loop expr return clause* ())))))
    
    ((match-aux "match" loop expr return () ((and-let-clause* guard-expr body) ...))
     (or (and-let* and-let-clause*
	   (and guard-expr
		(call-with-values
		    (lambda () . body)
		  return)))
	 ...
	 (if #f #f)))

    ((match-aux "match" loop expr return (clause . clause*) compiled-clauses*)
     (match-aux "clause" loop expr clause
		("receive-clause" (loop expr return clause* compiled-clauses*))))

    ((match-aux "receive-clause" (loop expr return clause* (compiled-clause1 ...))
		compiled-clause2)
     (match-aux "match" loop expr return clause*
		(compiled-clause1 ... compiled-clause2)))

    ((match-aux "clause" loop expr (pattern (guard guard-expr) . body) (k ...))
     (match-aux "guarded-clause" loop expr (pattern guard-expr body) (k ...)))

    ((match-aux "clause" loop expr (pattern . body) (k ...))
     (match-aux "guarded-clause" loop expr (pattern #t body) (k ...)))

    ((match-aux "guarded-clause" loop expr (pattern guard-expr body) (k ...))
     (match-aux "pattern" loop expr pattern
		("receive-pattern" ((k ...) guard-expr body))))

    ((match-aux "receive-pattern" ((k ...) guard-expr body) and-let-clause*)
     (match-aux k ... (and-let-clause* guard-expr body)))

    ((match-aux "pattern" loop expr ,(cata -> var ...) (k ...))
     (match-aux "cata" loop expr cata (var ...) (k ...)))
    
    ((match-aux "pattern" loop expr ,(var ...) (k ...))
     (match-aux "cata" loop expr loop (var ...) (k ...)))

    ((match-aux "cata" loop expr cata (var ...) (k ...))
     (match-aux "cata" loop expr cata (var ...) () (k ...)))

    ((match-aux "cata" loop expr cata () ((var tmp) ...) (k ...))
     (match-aux k ... ((var #t) ...
		       ((begin (call-with-values (lambda () (cata expr))
				 (lambda (tmp ...)
				   (set! var tmp) ...))
			       #t)))))

    ((match-aux "cata" loop expr cata (var1 . var*) ((var tmp) ...) (k ...))
     (match-aux "cata" loop expr cata var* ((var tmp) ... (var1 tmp1)) (k ...)))
    
    ((match-aux "pattern" loop expr ,var (k ...))
     (match-aux k ... ((var #t)
		       ((begin (set! var expr)
			       #t)))))

    ((match-aux "pattern" loop expr (pattern1 pattern2 ... . pattern*) (k ...))
     (match-aux "list-pattern" loop expr (pattern1 pattern2 ... . pattern*) () (k ...)))

    ((match-aux "pattern" loop expr literal (k ...))
     (match-aux k ... (((eq? expr 'literal)))))

    ((match-aux "list-pattern" loop expr () (and-let-clause ...) (k ...))
     (match-aux k ... (and-let-clause ... ((null? expr)))))

    ((match-aux "list-pattern" loop expr ,pattern
		and-let-clause* (k ...))
     (match-aux "pattern" loop expr ,pattern
		("receive-tail-pattern" ((k ...) and-let-clause*))))
    
    ((match-aux "list-pattern" loop expr (pattern . pattern*)
		(and-let-clause ...) (k ...))
     (match-aux "pattern" loop e pattern
		("receive-list-pattern" ((k ...) loop f pattern*
					 (and-let-clause
					  ...
					  ((pair? expr))
					  (e #t)
					  ((begin (set! e (car expr))
						  #t))
					  (f #t)
					  ((begin (set! f (cdr expr))
						  #t)))))))

    ((match-aux "list-pattern" loop expr pattern
		and-let-clause* (k ...))
     (match-aux "pattern" loop expr pattern
		("receive-tail-pattern" ((k ...) and-let-clause*))))

    ((match-aux "receive-tail-pattern" ((k ...) (and-let-clause ...))
		and-let-clause*)
     (match-aux k ... (and-let-clause ... . and-let-clause*))) 

    ((match-aux "receive-list-pattern" ((k ...) loop expr pattern*
					(and-let-clause ...))
		and-let-clause*)
     (match-aux "list-pattern" loop expr pattern*
		(and-let-clause ... . and-let-clause*)
		(k ...)))

     

    
    ))
