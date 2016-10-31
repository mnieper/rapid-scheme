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
     (let ((expr e))
       (call-with-current-continuation
	(lambda (return)
	  (match-aux "match" expr return clause* ())))))
    
    ((match-aux "match" expr return () ((and-let-clause* guard-expr body) ...))
     (or (and-let* and-let-clause*
	   (and guard-expr
		(call-with-values
		    (lambda () . body)
		  return)))
	 ...
	 (if #f #f)))
    ((match-aux "match" expr return (clause . clause*) compiled-clauses*)
     (match-aux "clause" expr clause
		("receive-clause" (expr return clause* compiled-clauses*))))

    ((match-aux "receive-clause" (expr return clause* (compiled-clause1 ...))
		compiled-clause2)
     (match-aux "match" expr return clause* (compiled-clause1 ... compiled-clause2)))

    ((match-aux "clause" expr (pattern (guard guard-expr) . body) (k ...))
     (match-aux "guarded-clause" expr (pattern guard-expr body) (k ...)))

    ((match-aux "clause" expr (pattern . body) (k ...))
     (match-aux "guarded-clause" expr (pattern #t body) (k ...)))

    ((match-aux "guarded-clause" expr (pattern guard-expr body) (k ...))
     (match-aux "pattern" expr pattern ("receive-pattern" ((k ...) guard-expr body))))

    ((match-aux "receive-pattern" ((k ...) guard-expr body) and-let-clause*)
     (match-aux k ... (and-let-clause* guard-expr body)))

    ((match-aux "pattern" expr ,var (k ...))
     (match-aux k ... ((var #t)
		       ((begin (set! var expr)
			       #t)))))
    
    ((match-aux "pattern" expr literal (k ...))
     (match-aux k ... (((eq? expr 'literal)))))
     
    ))
