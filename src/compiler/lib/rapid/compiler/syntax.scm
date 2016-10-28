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
  (datum unwrap-syntax syntax-set-datum!)
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
	datum)))

(define (derive-syntax datum context)
 (if (syntax? datum)
     (make-syntax (unwrap-syntax datum) context)
     (receive (syntax datum-map)
	 (let loop1 ((datum datum)
		     (datum-map (make-imap eq?)))
	   (define (failure)
	     (cond
	      ((pair? datum)
	       (let* ((syntax-pair (make-syntax (cons #f '()) context))
		      (datum-map (imap-replace datum-map datum syntax-pair)))
		 (receive (syntax-element* datum-map)
		     (let loop2 ((datum datum)
				 (datum-map datum-map))
		       (cond
			((null? datum)
			 (values '() datum-map))
			
			((pair? datum)
			 (receive (syntax-element datum-map)
			     (loop1 (car datum) datum-map)
			   (receive (syntax-element* datum-map)
			       (loop2 (cdr datum) datum-map)
			     (values (cons syntax-element syntax-element*)
				     datum-map))))
			(else
			 (loop1 datum datum-map))))
		   (syntax-set-datum! syntax-pair syntax-element*)
		   (values syntax-pair datum-map))))
	     ((vector? datum)
	       ;; FIXME
	       (error "not implemented"))
	      ((symbol? datum)
	       (values (make-syntax (make-synthetic-identifier datum) context) datum-map))
	      (else
	       (values (make-syntax datum context) datum-map))))
	   (define (success syntax)
	     (write-shared syntax) (newline)
	     (values syntax datum-map))
	   (if (syntax? datum) 
	       (values datum datum-map)
	       (imap-ref datum-map datum failure success)))
       syntax)))

(define-syntax syntax-match
  (syntax-rules ()
    ((match `expr clause ...)
     (let ((e expr))
       (call-with-current-continuation
	(lambda (r)
	  (match-aux "or" `e r clause ... ())))))))

;; XXX: let* alone is not sufficient; and-let* neither
;; XXX: Where to automatically pack/unpack? -> should happen in match?
;; `could be the packer. What about the unpacker?

(define-syntax match-aux
  (syntax-rules (unquote =>)
    ((match-aux "or" `e r () ((ac* proc) ...))
     (or (let*
	     ac*
	   (call-with-current-continuation
	    (lambda (skip)	   
	      (call-with-values
		  (lambda ()
		    (proc (lambda () (skip #f))))
		r)))) ...
	 (if #f #f)))
    ((match-aux "or" `e r (c . c*) tc*)
     (match-aux "clause" c `e r c* tc*))
    ((match-aux "clause" (p => proc) `e r c* tc*)
     (match-aux "=>" (p proc) `e r c* tc*))
    ((match-aux "clause" (p . body) `e r c* tc*)
     (match-aux "=>" (p (lambda (skip) . body)) `e r c* tc*))
    ((match-aux "=>" (,x proc) `e r c* (tc ...))
     (match-aux "or" `e r c* (tc ... (((x e) proc)))))))
		 
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
