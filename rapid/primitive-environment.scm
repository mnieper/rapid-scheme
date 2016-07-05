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
       (delay
	 ;; TODO: Rename the literal if the identifier is bound and a closure.
	 (make-literal (syntax->datum (list-ref form 1)) syntax)))))

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

  ;; define-syntax-parameter syntax
  (define-transformer (define-syntax-parameter syntax)
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
       (make-parameterized-transformer
	(lambda (syntax)
	  (and-let*
	      ((transformed-syntax (transformer syntax)))
	    (expand-syntax! transformed-syntax)))
	transformer-syntax)
       syntax)))

  (define-transformer (syntax-parameterize syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (<= 3 (length form))
	      (raise-syntax-error syntax "bad syntax-parameterize syntax")))
	 (binding-syntax* (unwrap-syntax (list-ref form 1)))
	 ((or (list? binding-syntax*)
	      (raise-syntax-error (list-ref form 1)
				  "bad syntax-parameterize syntax")))
	 (denotation+old-proc+old-syntax+new-proc+new-syntax*
	  (let loop ((binding-syntax* binding-syntax*))
	    (if (null? binding-syntax*)
		'()
		(or (and-let*
			((binding-syntax (car binding-syntax*))
			 (binding (unwrap-syntax binding-syntax))
			 ((or (and (list? binding)
				   (= 2 (length binding)))
			      (raise-syntax-error binding-syntax
						  "bad syntax binding")))
			 (keyword-syntax (car binding))
			 (transformer-spec-syntax (cadr binding))
			 (keyword (unwrap-syntax keyword-syntax))
			 ((identifier? keyword))
			 (denotation (lookup-denotation! keyword-syntax))
			 ((or (parameterized-transformer? denotation)
			      (begin
				(raise-syntax-error
				 keyword-syntax
				 "identifier ‘a’ does not denote a syntax parameter"
				 (identifier->symbol keyword))
				(raise-syntax-note
				 (lookup-syntax! keyword-syntax)
				 "identifier ‘a’ was bound here"
				 (identifier->symbol keyword)))))
			 (transformer (expand-transformer transformer-spec-syntax))
			 (proc
			  (lambda (syntax)
			    (and-let*
				((transformed-syntax (transformer syntax)))
			      (expand-syntax! transformed-syntax)))))
		      (cons (vector denotation
				    (transformer-proc denotation)
				    (transformer-syntax denotation)
				    proc
				    transformer-syntax)
			    (loop (cdr binding-syntax*))))
		    (loop (cdr binding-syntax*)))))))
      (dynamic-wind
	  (lambda ()
	    (for-each (lambda (vector)
			(transformer-set-proc!
			 (vector-ref vector 0)
			 (vector-ref vector 3))
			(transformer-set-syntax!
			 (vector-ref vector 0)
			 (vector-ref vector 4)))
		      denotation+old-proc+old-syntax+new-proc+new-syntax*))
	  (lambda ()
	    (expand-into-expression
	     (delay
	       (with-scope
		 (expand-body (list-tail form 2) syntax)))))
	  (lambda ()
	    (for-each (lambda (vector)
			(transformer-set-proc!
			 (vector-ref vector 0)
			 (vector-ref vector 1))
			(transformer-set-syntax!
			 (vector-ref vector 0)
			 (vector-ref vector 2)))
		      denotation+old-proc+old-syntax+new-proc+new-syntax*)))))

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
					 (syntax->datum identifier-syntax))
		     (raise-syntax-note (binding-syntax syntactic-binding)
					"identifier ‘~a’ was bound here"
					(syntax->datum identifier-syntax)))))
	 (denotation (binding-denotation syntactic-binding))
	 ((or (location? denotation)
	      (begin (raise-syntax-error identifier-syntax
					 "not a variable")
		     (raise-syntax-note (lookup-syntax! identifier-syntax)
					"identifier ‘~a’ was bound here"
					(unwrap-syntax identifier-syntax))))))
      (expand-into-expression
       (delay
	 (make-assignment denotation (expand-expression (list-ref form 2)) syntax)))))

  ;; if syntax
  (define-transformer (if syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 (length (length form))
	 ((or (or (= length 3) (= length 4))
	      (raise-syntax-error syntax "bad if syntax")))
	 (test-syntax (list-ref form 1))
	 (consequent-syntax (list-ref form 2)))
      (let ((alternate-syntax (and (= length 4)
				   (list-ref form 3))))
	(expand-into-expression
	 (delay
	   (make-conditional (expand-expression test-syntax)
			     (expand-expression consequent-syntax)
			     (if alternate-syntax
				 (expand-expression alternate-syntax)
				 (make-undefined #f))
			     syntax))))))

  ;; case-lambda syntax
  (define-transformer (case-lambda syntax)
    (and-let*
	((form (unwrap-syntax syntax)))
      (expand-into-expression
       (delay
	 (make-procedure
	  (let loop ((clause-syntax* (cdr form)))
	    (if (null? clause-syntax*)
		'()
		(or (and-let*
			((clause-syntax (car clause-syntax*))
			 (clause (unwrap-syntax clause-syntax))
			 ((or (and (not (null? clause)) (list? clause))
			      (raise-syntax-error clause-syntax
						  "bad case-lambda clause"))))
		      (with-scope
			(and-let*
			    ((formals (expand-formals! (car clause))))
			  (cons
			   (make-clause formals
					(list (expand-body (cdr clause)
							   clause-syntax))
					clause-syntax)
			   (loop (cdr clause-syntax*))))))
		    (loop (cdr clause-syntax*)))))
	  syntax)))))
  
  ;; include syntax
  (define-transformer (include syntax)
    (expand-include syntax #f))

  ;; include-ci syntax
  (define-transformer (include-ci syntax)
    (expand-include syntax #t))

  ;; define-record-type syntax
  (define-transformer (define-record-type syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (>= (length form) 4)
	      (raise-syntax-error syntax "bad define-record-type syntax")))
	 (rtd-syntax (list-ref form 1))
	 ((identifier-syntax? rtd-syntax))
	 (constructor-syntax (list-ref form 2))
	 (constructor (unwrap-syntax constructor-syntax))
	 ((or (and (not (null? constructor)) (list? constructor))
	      (raise-syntax-error constructor-syntax "bad constructor spec")))
	 ((every identifier-syntax? (cdr constructor)))
	 (predicate-syntax (list-ref form 3))
	 ((identifier-syntax? predicate-syntax))
	 (field-syntax* (list-tail form 4))
	 (field-table (imap identifier-comparator))
	 (identifier-table (imap identifier-comparator))
	 (index 0)
	 ((every
	   (lambda (field-syntax)
	     (and-let*
		 ((field (unwrap-syntax field-syntax))
		  ((or (list? field)
		       (raise-syntax-error field-syntax "bad field spec")))
		  (length (length field))
		  ((or (= length 2) (= length 3)
		       (raise-syntax-error field-syntax "invalid field spec")))
		  (field-name-syntax (list-ref field 0))
		  ((identifier-syntax? field-name-syntax))
		  (field-name (unwrap-syntax field-name-syntax))
		  (accessor-syntax (list-ref field 1))
		  ((identifier-syntax? accessor-syntax))
		  (accessor (unwrap-syntax accessor-syntax))
		  (mutator-syntax (if (= length 3) (list-ref field 2) #t))
		  (mutator (if (not (eq? mutator-syntax #t)) (unwrap-syntax mutator-syntax) #t))
		  ((or (eq? mutator-syntax #t)
		       (identifier-syntax? mutator-syntax)))
		  ((or (not (imap-ref/default field-table field-name #f))
		       (begin (raise-syntax-error field-name-syntax
						  "duplicate field name ‘~a’"
						  (identifier->symbol field-name))
			      (raise-syntax-note (vector-ref (imap-ref field-table field-name)
							     0)
						 "previous occurrence was here"))))
		  ((or (not (imap-ref/default identifier-table accessor #f))
		       (begin (raise-syntax-error accessor-syntax
						  "duplicate identifier ‘~a’"
						  (identifier->symbol accessor))
			      (raise-syntax-note (imap-ref identifier-table accessor)
						 "previous occurrence was here"))))
		  ((or (eq? mutator #t)
		       (not (imap-ref/default identifier-table accessor #f))
		       (begin (raise-syntax-error mutator-syntax
						  "duplicate identifier ‘~a’"
						  (identifier->symbol mutator))
			      (raise-syntax-note (imap-ref identifier-table mutator)
						 "previous occurrence was here")))))
	       (set! field-table
		     (imap-replace field-table field-name (vector field-name-syntax index)))
	       (set! index (+ 1 index))
	       (set! identifier-table 
		     (imap-replace identifier-table accessor accessor-syntax))
	       (unless (eq? mutator #t)
		 (set! identifier-table
		       (imap-replace identifier-table mutator mutator-syntax)))
	       #t))
	   field-syntax*))
	 (arg-table (imap identifier-comparator))
	 ((every
	   (lambda (identifier-syntax)
	     (and-let*
		 ((identifier (unwrap-syntax identifier-syntax))
		  ((or (not (imap-ref/default arg-table identifier #f))
		       (begin (raise-syntax-error identifier-syntax
						  "duplicate field name ‘~a’ in constructor spec"
						  (identifier->symbol identifier))
			      (raise-syntax-note (vector-ref (imap-ref arg-table identifier) 0)
						 "previous occurrence was here"))))
		  (syntax+index (imap-ref/default field-table identifier #f)))
	       (set! arg-table
		     (imap-replace arg-table identifier (vector identifier-syntax
								(vector-ref syntax+index 1))))
	       #t))
	   (cdr constructor))))
      (receive (rtd-location* _)
	  (expand-into-definition (list rtd-syntax) '() rtd-syntax
				  (delay
				    (make-procedure-call (make-primitive-reference
							  (make-primitive 'make-rtd
									  rtd-syntax)
							  rtd-syntax)
							 (list (make-literal
								(length field-syntax*)
								rtd-syntax))
							 rtd-syntax))
				  rtd-syntax)
	(expand-into-definition (list (car constructor)) '() (car constructor)
				(delay
				  (make-procedure-call (make-primitive-reference
							(make-primitive 'make-constructor
									(car constructor))
							(car constructor))
						       (list (make-reference (car rtd-location*)
									     constructor-syntax)
							     (make-literal
							      (list->vector
							       (map
								(lambda (identifier-syntax)
								  (vector-ref
								   (imap-ref field-table
									     (unwrap-syntax
									      identifier-syntax))
								   1))
								(cdr constructor)))
							      constructor-syntax))
						       constructor-syntax))
				constructor-syntax)
	(expand-into-definition (list predicate-syntax) '() predicate-syntax
				(delay
				  (make-procedure-call (make-primitive-reference
							(make-primitive 'make-predicate
									predicate-syntax)
							predicate-syntax)
						       (list (make-reference (car rtd-location*)
									     predicate-syntax))
						       predicate-syntax))
				predicate-syntax)
	(for-each
	 (lambda (field-syntax)
	   (let*
	       ((field (unwrap-syntax field-syntax))
		(index (vector-ref (imap-ref field-table (unwrap-syntax (list-ref field 0)))
				   1)))
	     (expand-into-definition
	      (list (list-ref field 1)) '() (list-ref field 1)
	      (delay
		(make-procedure-call (make-primitive-reference
				      (make-primitive 'make-accessor
						      (list-ref field 1))
				      (list-ref field 1))
				     (list (make-reference (car rtd-location*)
							   (list-ref field 1))
					   (make-literal index
							 (list-ref field 1)))
				     (list-ref field 1)))
	      (list-ref field 1))
	     (unless (null? (cddr field))
	       (expand-into-definition
		(list (list-ref field 2)) '() (list-ref field 2)
		(delay
		  (make-procedure-call (make-primitive-reference
					(make-primitive 'make-mutator
							(list-ref field 1))
					(list-ref field 1))
				       (list (make-reference (car rtd-location*)
							     (list-ref field 2))
					     (make-literal index
							   (list-ref field 2)))
				       (list-ref field 2)))
		(list-ref field 2)))))
	 field-syntax*))))

  (define-transformer (rapid-features syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 1)
	      (raise-syntax-error syntax "bad features syntax"))))
      (expand-into-expression
       (delay
	 (make-literal (features) syntax)))))

  (define-transformer (cond-expand syntax)
    (and-let*
	((expanded-clause (expand-cond-expand syntax))
	 ((not (null? expanded-clause))))
      (expand-into-sequence expanded-clause syntax))))

;;; Utility functions

(define (expand-formals! formals-syntax)
  (unpack-formals formals-syntax
		  (lambda (fixed rest*)
		    (make-formals
		     (map expand-formal! fixed)
		     (map expand-formal! rest*)
		     formals-syntax))))

(define (expand-formal! syntax)
  (let ((location (make-location syntax)))
    (insert-syntactic-binding! syntax location)
    location))

(define (expand-include syntax ci?)    
  (and-let*
      ((form (unwrap-syntax syntax))
       (syntax* (cdr form))
       ((every (lambda (syntax)
		 (or (string? (unwrap-syntax syntax))
		     (raise-syntax-error syntax "not a string")))
	       syntax*)))
    (expand-into-sequence (generator->list (read-file* syntax* ci?)) syntax)))

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
       (fixed (list-queue)))
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
	    (success (list-queue-list fixed) (list formals))
	    (success (list-queue-list fixed) '())))))))

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
