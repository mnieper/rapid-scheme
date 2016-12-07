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
  (syntax-rules ..1 (unquote -> guard ...)
    
    ((match-aux e clause*)
     (let loop ((expr e))
       (call-with-current-continuation
	(lambda (return)
	  (match-aux "match" loop expr return clause* ())))))
    
    ((match-aux "match" loop expr return () (((var ..1) matcher guard-expr body) ..1))
     (begin
       (call-with-current-continuation
	(lambda (fail)
	  (define (failure) (fail #f))
	  (let-values (((var ..1) (matcher expr failure))) 
	    (unless guard-expr (failure))
	    (call-with-values (lambda () . body) return))) )
       ..1
       (if #f #f)))

    ((match-aux "match" loop expr return (clause . clause*) compiled-clauses*)
     (match-aux "clause" loop expr clause
		("receive-clause" (loop expr return clause* compiled-clauses*))))

    ((match-aux "receive-clause" (loop expr return clause* (compiled-clause1 ..1))
		compiled-clause2)
     (match-aux "match" loop expr return clause*
		(compiled-clause1 ..1 compiled-clause2)))

    ((match-aux "clause" loop expr (pattern (guard guard-expr) . body) (k ..1))
     (match-aux "guarded-clause" loop expr (pattern guard-expr body) (k ..1)))

    ((match-aux "clause" loop expr (pattern . body) (k ..1))
     (match-aux "guarded-clause" loop expr (pattern #t body) (k ..1)))

    ((match-aux "guarded-clause" loop expr (pattern guard-expr body) (k ..1))
     (match-aux "pattern" loop pattern
		("receive-pattern" ((k ..1) guard-expr body))))

    ((match-aux "receive-pattern" ((k ..1) guard-expr body) var* matcher)
     (match-aux k ..1 (var* matcher guard-expr body)))

    ((match-aux "pattern" loop ,(cata -> var ..1) (k ..1))
     (match-aux "cata" cata (var ..1) (k ..1)))

    ((match-aux "pattern" loop ,(var ..1) (k ..1))
     (match-aux "cata" loop (var ..1) (k ..1)))

    ((match-aux "cata" cata (var ..1) (k ..1))
     (match-aux k ..1 (var ..1) (lambda (expr failure) (cata expr))))
    
    ((match-aux "pattern" loop ,var (k ..1))
     (match-aux k ..1 (var) (lambda (expr failure) expr)))

    ((match-aux "pattern" loop (pattern1 ... pattern2 ..1 . pattern*) (k ..1))
     (match-aux "pattern" loop (pattern2 ..1 . pattern*)
		("ellipsis-pattern1" ((k ..1) loop (length '(pattern2 ..1)) pattern1))))

    ((match-aux "ellipsis-pattern1" ((k ..1) loop len pattern1) var* matcher*)
     (match-aux "pattern" loop pattern1
		("ellipsis-pattern2" ((k ..1) loop len var* matcher*))))

    ((match-aux "ellipsis-pattern2" ((k ..1) loop len var* matcher*) (var ..1) matcher)
     (match-aux k ..1 (var ..1 . var*)
		(lambda (expr failure)
		  (let-values (((head tail) (split expr len failure)))		    
		    (let-values ((v* (matcher* tail failure)))
		      (let-values (((var ..1) (map-values (lambda (expr)
							    (matcher expr failure))
							  head (length '(var ..1)))))
			(apply values var ..1 v*)))))))
    
    ((match-aux "pattern" loop (pattern1 pattern2 ..1 . pattern*) (k ..1))
     (match-aux "pattern" loop (pattern2 ..1 . pattern*)
		("list-pattern1" ((k ..1) loop pattern1))))

    ((match-aux "list-pattern1" ((k ..1) loop pattern1) var2 matcher2)
     (match-aux "pattern" loop pattern1
		("list-pattern2" ((k ..1) loop var2 matcher2))))

    ((match-aux "list-pattern2" ((k ..1) loop var2 matcher2) (var ..1) matcher1)
     (match-aux k ..1 (var ..1 . var2)
		(lambda (expr failure)
		  (unless (pair? expr) (failure))
		  (let-values (((var ..1) (matcher1 (car expr) failure)))
		    (let-values ((v* (matcher2 (cdr expr) failure)))
		      (apply values var ..1 v*))))))

    ((match-aux "pattern" loop () (k ..1))
     (match-aux k ..1 () (lambda (expr failure) (unless (null? expr) (failure)) (values))))
    
    ((match-aux "pattern" loop literal (k ..1))
     (match-aux k ..1 ()
		   (lambda (expr failure) (unless (eq? 'literal expr) (failure)) (values))))

    
    ))


(define (split flist i failure)
  (let loop ((i i) (end flist))
    (if (zero? i)
	(let loop ((flist flist) (end end))
	  (if (pair? end)
	      (let-values (((head tail)
                            (loop (cdr flist) (cdr end))))
		(values (cons (car flist) head) tail))
	      (values '() flist)))
	(if (pair? end)
	    (loop (- i 1) (cdr end))
	    (failure)))))

(define (map-values proc lst . len*)
  (if (null? lst)
      (apply values (make-list (car len*) '()))  
      (apply values
	     (apply map list 
		    (map (lambda (obj)
			   (let-values ((x* (proc obj)))
			     x*))
			 lst)))))
