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

(define-record-type <syntax>
  (make-syntax datum context)
  syntax?
  (datum unwrap-syntax)
  (context syntax-context))

(define (syntax->datum syntax)
  (if (syntax? syntax)
      (receive (datum syntax-map)
	  (let loop1 ((syntax syntax) (syntax-map (make-imap eq?)))
	    (cond
	     ((imap-ref/default syntax-map syntax #f)
	      => (lambda (datum)
		   datum))
	     (else
	      (let ((datum (unwrap-syntax syntax)))
		(cond
		 ((vector? datum)
		  (let* ((n (vector-length datum))
			 (vector (make-vector n)))
		    (let loop2 ((i 0)
				(syntax-map (imap-replace syntax-map syntax vector)))
		      (if (= i n)
			  (values vector syntax-map)
			  (receive (element syntax-map)
			      (loop1 (vector-ref datum i) syntax-map)
			    (vector-set! vector i element)
			    (loop2 (+ i 1) syntax-map))))))
		 ((pair? datum)
		  (let ((list (cons #f '())))
		    (receive (element syntax-map)
			(loop1 (car datum) (imap-replace syntax-map syntax list))
		      (set-car! list element)
		      (let loop2 ((datum datum)
				  (pair list)
				  (syntax-map syntax-map))
			(if (not (pair? (cdr datum)))
			    (if (null? (cdr datum))
				(values list syntax-map)
				(receive (element syntax-map)
				    (loop1 (cdr datum) syntax-map)
				  (set-cdr! pair element)
				  (values list syntax-map)))
			    (receive (element syntax-map)
				(loop1 (cadr datum) syntax-map)
			      (set-cdr! pair (cons element '()))
			      (loop2 (cdr datum) (cdr pair) syntax-map)))))))
		 ((identifier? datum)
		  (values (identifier->symbol datum) syntax-map))
		 (else
		  (values datum syntax-map)))))))
	datum)
      syntax))

(define (derive-syntax datum context)
  ;; TODO: Handle vectors and self-referential data structures
  (if (syntax? datum)
      (make-syntax (unwrap-syntax datum) context)
      (let loop1 ((datum datum))
	(cond
	 ((syntax? datum)
	  datum)
	 ((pair? datum)
	  (make-syntax
	   (let loop2 ((datum datum))
	     (cond
	      ((null? datum)
	       '())
	      ((pair? datum)
	       (cons (loop1 (car datum)) (loop2 (cdr datum))))
	      (else
	       (loop1 datum))))
	   context))
	 (else
	  (make-syntax datum context))))))

(define-syntax syntax-match
  (syntax-rules ()
    ((match `syntax clause ...)
     (let ((e (unwrap-syntax syntax))
	   (c (syntax-context syntax)))
       (call-with-current-continuation
	(lambda (r)
	  (match-aux "or" `e c r (clause ...) ())))))))

;; problematisch: mehrere Rückgabewerte; welches sind die Syntax-Einträge?
(define-syntax match-aux
  (syntax-rules (unquote =>)
    ((match-aux "or" `e c r () ((s ac* lc* body) ...))
     (or (and-let*
	     ac*
	   (let lc*
	       (call-with-current-continuation
		(lambda (skip)
		  (define (s) (skip #f))
		  (call-with-values
		      (lambda () . body)
		    r)
		  c))))
	 ...
	 (if #f #f)))
    ((match-aux "or" `e c r (cl . cl*) tc*)
     (match-aux "clause" cl `e c r cl* tc*))
    ((match-aux "clause" (p (=> s) . body) `e c r cl* tc*)
     (match-aux "=>" (p s body) `e c r cl* tc*))
    ((match-aux "clause" (p . body) `e c r cl* tc*)
     (match-aux "=>" (p s body) `e c r cl* tc*))
    ((match-aux "=>" (,x s body) `e c r cl* (tc ...))
     (match-aux "or" `e c r cl* (tc ... (s () ((x e)) body))))))
		 
     #;(let-syntax
	 ((qq (syntax-rules ..2 ()
		((qq template) `template))))
       (let-syntax
	   ((quasiquote
	     (syntax-rules ..3 (... unquote)
	       ((quasiquote (,a ... . b))
		(qq (,@a . b)))
	       ((quasiquote x)
		(qq x)))))
	 . body))
