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

;;; The environment exported by the built-in library ‘(rapid primitive)’

(define-syntactic-environment primitive-environment

  ;; ... syntax
  
  (define-auxiliary-syntax ...)

  ;; _ syntax
  
  (define-auxiliary-syntax _)

  ;; quote syntax
  
  (define-transformer (quote syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 2)
	      (begin (raise-syntax-error syntax
					 "bad quote syntax")
		     #f))))
      (expand-into-expression
       ;; TODO: Rename the literal if the identifier is bound and a closure.
       (make-literal (syntax->datum (list-ref form 1)) syntax))))

  ;; define-values syntax
  
  (define-transformer (define-values syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 3)
	      (begin (raise-syntax-error syntax
					 "bad define-values syntax")
		     #f))))
      (unpack-formals (list-ref form 1)
		      (lambda (fixed rest)
			(expand-into-definition fixed
						rest
						(list-ref form 1)
						(list-ref form 2)
						syntax)))))

  ;; define-syntax syntax

  (define-transformer (define-syntax syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 3)
	      (raise-syntax-error syntax "bad define-syntax syntax")))
	 (keyword-syntax (list-ref form 1))
	 ((identifier-syntax? keyword-syntax))
	 (transformer-syntax (list-ref form 2))
	 (transformer (expand-transformer transformer-syntax)))    
      (expand-into-syntax-definition
       keyword-syntax
       (make-transformer
	(lambda (syntax)
	  (and-let*
	      ((transformed-syntax (transformer syntax)))
	    (expand-syntax! transformed-syntax)))
	transformer-syntax)
       syntax)))

  ;; syntax-rules syntax

  (define-transformer (syntax-rules syntax)
    (and-let*
	((transformer
	  (unwrap-syntax syntax))
	 ((or (>= (length transformer) 2)
	      (raise-syntax-error syntax "bad syntax-rules syntax")))
	 (ellipsis-syntax*
	  (if (and (>= (length transformer) 3)
		   (identifier? (unwrap-syntax (list-ref transformer 1))))
	      (list (list-ref transformer 1))
	      '()))
	 (transformer-rest
	  (if (null? ellipsis-syntax*)
	      (list-tail transformer 1)
	      (list-tail transformer 2)))
	 (literal-syntax*-syntax
	  (car transformer-rest))
	 (literal-syntax* (unwrap-syntax literal-syntax*-syntax))
	 ((or (list? literal-syntax*)
	      (raise-syntax-error literal-syntax*-syntax "list expected")))
	 (syntax-rules-syntax*
	  (cdr transformer-rest))
	 (ellipsis* (map unwrap-syntax ellipsis-syntax*))
	 (free-identifier-comparator
	  (with-scope
	    (unless (null? ellipsis*)
	      (insert-syntactic-binding! (car ellipsis-syntax*)
					 (make-location #f)))
	    (make-free-identifier-comparator)))
	 (free-identifier=?
	  (comparator-equality-predicate free-identifier-comparator))
	 (literals
	  (let loop ((literals
		      (imap free-identifier-comparator))
		     (literal-syntax*
		      literal-syntax*))
	    (cond
	     ((null? literal-syntax*)
	      literals)
	     ((and-let*
		  ((literal-syntax (car literal-syntax*))
		   ((identifier-syntax? literal-syntax))
		   (literal (unwrap-syntax literal-syntax))
		   ((cond
		     ((imap-ref/default literals literal #f)
		      => (lambda (previous-syntax)
			   (raise-syntax-error literal-syntax
					       "duplicate literal identifier")
			   (raise-syntax-note previous-syntax
					      "previous occurrence was here")))
		     (else
		      #t)))
		   literal-syntax))
	      => (lambda (literal-syntax)
		   (loop (imap-replace literals
				       (unwrap-syntax literal-syntax)
				       literal-syntax)
			 (cdr literal-syntax*))))
	     (else
	      (loop literals (cdr literal-syntax*))))))
	 (ellipsis?
	  (lambda (identifier)
	    (and (not (literal? identifier))
		 (if (null? ellipsis*)
		     (and-let*
			 ((binding (syntactic-environment-ref
				    (current-syntactic-environment)
				    identifier))
			  (denotation (binding-denotation binding))
			  ((primitive-transformer? denotation)))
		       (eq? '... (primitive-transformer-name denotation)))
		     (free-identifier=? identifier (car ellipsis*))))))
	 (literal?
	  (lambda (identifier)
	    (and (imap-ref/default literals identifier #f)
		 #t)))
	 (underscore?
	  (lambda (identifier)
	    (and-let*
		((binding (syntactic-environment-ref (current-syntactic-environment)
							 identifier))
		 (denotation (binding-denotation binding))
		 ((primitive-transformer? denotation)))
	      (eq? '_ (primitive-transformer-name denotation))))))
      (expand-into-transformer (make-syntax-rules-transformer ellipsis?
							      literal?
							      underscore?
							      syntax-rules-syntax*
							      syntax)
			       syntax)))

  ;; syntax-error syntax
  (define-transformer (syntax-error syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (>= (length form) 2)
	      (raise-syntax-error syntax "bad syntax-error syntax")))
	 (message (unwrap-syntax (cadr form)))
	 ((or (string? message)
	      (raise-syntax-error (cadr form) "not a string literal")))
	 (port (open-output-string)))
      (display message port)
      (when (> (length form) 2)
	(display ":" port)
	(do ((irritant-syntax* (cddr form) (cdr irritant-syntax*)))
	    ((null? irritant-syntax*))
	  (display " " port)
	  (display (syntax->datum (car irritant-syntax*)) port)))
      (raise-syntax-error syntax (get-output-string port))))
  
  ;; define-primitive syntax
  
  (define-transformer (define-primitive syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 3)
	      (begin (raise-syntax-error syntax
					 "bad define-primitive syntax")
		     #f)))
	 (identifier-syntax (list-ref form 1))
	 (identifier (unwrap-syntax identifier-syntax))
	 ((or (identifier? identifier)
	      (begin (raise-syntax-error identifier-syntax
					 "identifier expected")
		     #f)))
	 (literal-syntax (list-ref form 2))
	 (literal (expand-expression literal-syntax))
	 ((or (literal? literal)
	      (begin (raise-syntax-error literal-syntax
					 "literal symbol expected")
		     #f)))
	 (symbol (literal-datum literal))
	 ((or (symbol? symbol)
	      (begin (raise-syntax-error literal-syntax
					 "symbol expected")
		     #f))))
      (expand-into-syntax-definition identifier-syntax
				     (make-primitive symbol
						     literal-syntax)
				     syntax)))
  ;; begin syntax
  (define-transformer (begin syntax)
    (expand-into-sequence (cdr (unwrap-syntax syntax)) syntax))

  ;; set! syntax
  (define-transformer (set! syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 3)
	      (raise-syntax-error syntax "bad set! syntax")))
	 (identifier-syntax (list-ref form 1))
	 (syntactic-binding (lookup-syntactic-binding! identifier-syntax))
	 ((or (not (binding-immutable? syntactic-binding))
	      (begin (raise-syntax-error identifier-syntax
					 "identifier ‘~a’ is immutable"
					 (unwrap-syntax identifier-syntax))
		     (raise-syntax-note (binding-syntax syntactic-binding)
					"identifier ‘~a’ was bound here"
					(unwrap-syntax identifier-syntax)))))
	 (denotation (binding-denotation syntactic-binding))
	 ((or (location? denotation)
	      (begin (raise-syntax-error identifier-syntax
					 "not a variable")
		     (raise-syntax-note (lookup-syntax! identifier-syntax)
					"identifier ‘~a’ was bound here"
					(unwrap-syntax identifier-syntax))))))
      (expand-into-expression
       (make-assignment denotation (expand-expression (list-ref form 2)) syntax))))

  ;; if syntax
  (define-transformer (if syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 (length (length form))
	 ((or (or (= length 3) (= length 4))
	      (raise-syntax-error syntax "bad if syntax")))
	 (test-syntax (list-ref form 1))
	 (consequent-syntax (list-ref form 2))
	 (alternate-syntax (list-ref form 3)))
      (expand-into-expression
       (make-conditional (expand-expression test-syntax)
			 (expand-expression consequent-syntax)
			 (if alternate-syntax
			     (expand-expression alternate-syntax)
			     (make-undefined #f))
			 syntax))))

  ;; FIXME:
  ;; case-lambda
  ;; include
  ;; include-ci
  
  )

;;; Utility functions

(define (unpack-formals formals-syntax success)
  (define variables (imap identifier-comparator))

  (define (unique-variable? syntax)
    (and-let*
	(((identifier-syntax? syntax))
	 (identifier (unwrap-syntax syntax)))
      (cond
       ((imap-ref/default variables identifier #f)
	=> (lambda (previous-syntax)
	     (raise-syntax-error syntax
				 "duplicate parameter ‘~a’"
				 (identifier->symbol identifier))
	     (raise-syntax-note previous-syntax
				"previous appearance of ‘~a’ was here"
				(syntax->datum previous-syntax))
	     #f))
       (else
	(set! variables
	      (imap-replace variables identifier syntax))
	#t))))

  (and-let*
      ((formals
	(let ((datum (unwrap-syntax formals-syntax)))
	  (if (or (null? datum) (pair? datum))
	      datum
	      formals-syntax)))
       ((flist? formals))
       (fixed (list-queue))
       (let loop ((formals formals))
	 (cond
	  ((null? formals)
	   (success (list-queue-list fixed) '()))
	  ((pair? formals)
	   (when (unique-variable? (car formals))
	     (list-queue-add-back! fixed (car formals)))
	   (loop (cdr formals)))
	  (else
	   (if (unique-variable? formals)
	       (success (list-queue-list fixed) formals)
	       (success (list-queue-list fixed) '()))))))))

(define (identifier-syntax? syntax)
  (and-let*
      ((datum (unwrap-syntax syntax))
       ((or (identifier? datum)
	    (begin (raise-syntax-error syntax "bad identifier")
		   #f))))))

(define (flist? syntax)
  (or (not (circular-list? syntax))
      (begin (raise-syntax-error syntax "circular list in source")
	     #f)))
