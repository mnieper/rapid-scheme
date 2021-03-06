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

;;; Macros

(define-syntax with-syntactic-environment
  (syntax-rules ()
    ((with-syntactic-environment syntactic-environment
       body1 body2 ...)
     (parameterize ((current-syntactic-environment
		     syntactic-environment))
       body1 body2 ...))))

(define-syntax define-transformer
  (syntax-rules ()
    ((define-transformer . _)
     (syntax-error "invalid use of auxiliary syntax"))))

(define-syntax define-auxiliary-syntax
  (syntax-rules ()
    ((define-transformer . _)
     (syntax-error "invalid use of auxiliary syntax"))))

(define-syntax define-syntactic-environment
  (syntax-rules ()
    ((define-syntactic-environment environment . definitions)
     (define-syntactic-environment-helper environment () . definitions))))

(define-syntax define-syntactic-environment-helper
  (syntax-rules (define-transformer define-auxiliary-syntax)
    ((define-syntactic-environment-helper environment
       commands)
     (begin
       (define environment (make-syntactic-environment))
       (with-syntactic-environment environment
	 . commands)))
    ((define-syntactic-environment-helper
       environment (command ...)       
       (define-transformer (name syntax)
	 body1 body2 ...)
       . definitions)
     (define-syntactic-environment-helper
       environment
       (command
	... 
	(insert-syntactic-binding!
	 (derive-syntax (symbol->identifier 'name) #f)
	 (make-primitive-transformer (lambda (syntax)
			     body1 body2 ...)
			   'name)))
       . definitions))
    ((define-syntactic-environment-helper
       environment (command ...)
       (define-auxiliary-syntax name)
       . definitions)
     (define-syntactic-environment-helper
       environment
       (command
	...
	(insert-syntactic-binding!
	 (derive-syntax (symbol->identifier 'name) #f)
	 (make-primitive-transformer
	  (invalid-use-of-auxiliary-syntax 'name)
	  'name)))
       . definitions))))

(define-syntax with-scope
  (syntax-rules ()
    ((with-scope
       body1 body2 ...)
     (parameterize
	 ((current-syntactic-environment
	   (%make-syntactic-environment (current-bindings)
					(imap identifier-comparator))))
       body1 body2 ...))))
     
;;; Syntactic bindings

(define-record-type <syntactic-binding>
  (make-syntactic-binding binding-syntax denotation immutable?)
  syntactic-binding?
  (binding-syntax binding-syntax)
  (denotation binding-denotation)
  (immutable? binding-immutable?))

;;; Denotations

;; Variable bindings

(define-record-type <denotation>
  (make-denotation identity)
  denotation?
  (identity denotation-identity)
  (aux denotation-aux denotation-set-aux!))

(define-record-type (<location> <denotation>)
  (%make-location location-syntax identity)
  location?
  (location-syntax location-syntax))

(define make-location
  (case-lambda
   ((syntax) (%make-location syntax (generate-identity)))
   ((identity syntax) (%make-location syntax identity))))

(define-record-type (<primitive> <denotation>)
  (%make-primitive value syntax identity)
  primitive?
  (value primitive-value)
  (syntax primitive-syntax))

(define (make-primitive value syntax)
  (%make-primitive value syntax (generate-identity)))

(define-record-type (<transformer> <denotation>)
  (%make-transformer proc syntax identity)
  transformer?
  (proc transformer-proc transformer-set-proc!)
  (syntax transformer-syntax transformer-set-syntax!))

(define (make-transformer proc syntax)
  (%make-transformer proc syntax (generate-identity)))

(define-record-type (<parameterized-transformer> <transformer>)
  (%make-parameterized-transformer proc syntax identity)
  parameterized-transformer?)

(define (make-parameterized-transformer proc syntax)
  (%make-parameterized-transformer proc syntax (generate-identity)))


;; XXX
;; Instead of defining primitive values and transformers, we could simply give
;; them a name that distingishes these type.

(define-record-type (<primitive-transformer> <transformer>)
  (%make-primitive-transformer proc name syntax identity)
  primitive-transformer?
  (name primitive-transformer-name))

(define (make-primitive-transformer proc name)
  (%make-primitive-transformer proc name #f (generate-identity)))

(define (invalid-use-of-auxiliary-syntax name)
  (lambda (syntax)
    (raise-syntax-error syntax "invalid use of auxiliary-syntax ‘~a’" name)))

;;; Syntactic environments

(define-record-type <syntactic-environment>
  (%make-syntactic-environment bindings used-identifiers)
  syntactic-environment?
  (bindings syntactic-environment-bindings
	    syntactic-environment-set-bindings!)
  (used-identifiers used-identifiers set-used-identifiers!))

(define current-syntactic-environment (make-parameter #f))

(define current-bindings
  (case-lambda
   (() (syntactic-environment-bindings (current-syntactic-environment)))
   ((bindings)
    (syntactic-environment-set-bindings! (current-syntactic-environment)
					 bindings))))

(define current-used-identifiers
  (case-lambda
   (() (used-identifiers (current-syntactic-environment)))
   ((identifiers)
    (set-used-identifiers! (current-syntactic-environment) identifiers))))

(define (use-identifier! identifier binding)
  (current-used-identifiers (imap-replace (current-used-identifiers)
					  identifier
					  binding)))

(define (make-syntactic-environment)
  (%make-syntactic-environment (imap identifier-comparator)
			       (imap identifier-comparator)))

(define (syntactic-environment-for-each proc syntactic-environment)
  (imap-for-each proc
		 (syntactic-environment-bindings syntactic-environment)))

(define (export-syntactic-environment! environment exports)
  (exports-for-each
   (lambda (identifier export-spec)
     (and-let*
	 ((denotation
	   (syntactic-environment-lookup-denotation! environment
						     (export-spec-source
						      export-spec))))
       (insert-syntactic-binding! (export-spec-target export-spec) denotation #t)))
   exports))

(define (import-syntactic-environment! environment imports)
  (export-syntactic-environment! environment
				 (imports (syntactic-environment-bindings
					   environment))))

(define syntactic-environment-ref
  (case-lambda
   ((environment identifier)
    (syntactic-environment-ref environment identifier #f))
   ((environment identifier isolated?)
    (let loop ((id identifier) (environment environment))
      (cond
       ((imap-ref/default (syntactic-environment-bindings environment)
			  id
			  #f)
	=> (lambda (binding)
	     (unless isolated?
	       (use-identifier! identifier binding))
	     binding))
       (else
	(receive (id environment)
	    (unclose-syntax id)
	  (cond
	   (id
	    (loop id environment))
	   (else
	    (unless isolated?
	      (use-identifier! identifier #t))
	    #f)))))))))

(define (lookup-syntactic-binding! identifier-syntax)
  (or (syntactic-environment-ref (current-syntactic-environment)
				 (unwrap-syntax identifier-syntax))
      (raise-syntax-error identifier-syntax
			  "identifier ‘~a’ not bound"
			  (syntax->datum identifier-syntax))))

(define (lookup-denotation! identifier-syntax)
  (and-let*
      ((binding (lookup-syntactic-binding! identifier-syntax)))
    (binding-denotation binding)))

(define (lookup-syntax! identifier-syntax)
  (and-let*
      ((binding (lookup-syntactic-binding! identifier-syntax)))
    (binding-syntax binding)))

(define (syntactic-environment-lookup-denotation! environment identifier-syntax)
  (with-syntactic-environment environment
    (lookup-denotation! identifier-syntax)))

(define insert-syntactic-binding!
  (case-lambda
   ((identifier-syntax denotation)
    (insert-syntactic-binding! identifier-syntax denotation #f))
   ((identifier-syntax denotation immutable?)
    (let ((identifier (unwrap-syntax identifier-syntax)))
      (cond
       ((and-let*
	    ((binding
	      (imap-ref/default (current-used-identifiers)
				(unwrap-syntax identifier-syntax)
				#f))
	     ((or (eq? binding #t)
		  (not (eq? (binding-denotation binding) denotation)))))
	  binding)
	=> (lambda (binding)
	     (raise-syntax-error identifier-syntax
				 "meaning of identifier ‘~a’ cannot be changed"
				 (syntax->datum identifier-syntax))
	     (when (syntactic-binding? binding)
	       (raise-syntax-note (binding-syntax binding)
				  "identifier ‘~a’ was bound here"
				  (syntax->datum identifier-syntax)))
	     #f))
       (else
	(let ((binding
	       (make-syntactic-binding identifier-syntax denotation immutable?)))
	  (use-identifier! (unwrap-syntax identifier-syntax) binding)
	  (current-bindings (imap-replace (current-bindings)
					  identifier
					  binding)))))))))

(define (syntactic-environment-insert-binding! syntactic-environment
					       identifier-syntax
					       denotation)
  (with-syntactic-environment syntactic-environment
    (insert-syntactic-binding! identifier-syntax denotation)))

(define (identifier=? environment1 identifier1 environment2 identifier2)
  (let*
      ((binding1
	(syntactic-environment-ref environment1 identifier1 #t))
       (binding2
	(syntactic-environment-ref environment2 identifier2 #t))
       (denotation1
	(and binding1 (binding-denotation binding1)))
       (denotation2
	(and binding2 (binding-denotation binding2))))
    (cond
     ((and denotation1 denotation2)
      (eq? denotation1 denotation2))
     ((and (not denotation1) (not denotation2))
      (eq? (identifier->symbol identifier1)
	   (identifier->symbol identifier2)))
     (else
      #f))))

;;; free-identifier=?

(define (free-identifier=? identifier1 identifier2)
  (identifier=? (current-syntactic-environment) identifier1
		(current-syntactic-environment) identifier2))

(define (make-free-identifier-comparator)

  (define environment (current-syntactic-environment))
  
  (define (free-identifier=? identifier1 identifier2)
    (identifier=? environment identifier1
		  environment identifier2))
  
  (define (free-identifier<? identifier1 identifier2)
    (let*
	((binding1
	  (syntactic-environment-ref environment identifier1 #t))
	 (binding2
	  (syntactic-environment-ref environment identifier2 #t))
	 (denotation1
	  (and binding1 (binding-denotation binding1)))
	 (denotation2
	  (and binding2 (binding-denotation binding2))))
      (cond
       ((and denotation1 denotation2)
	(< (denotation-identity denotation1)
	   (denotation-identity denotation2)))
       ((and (not denotation1) denotation2))
       (else
	;; TODO: Make use of identifiers' hashes to speed up comparison.
	(string<? (symbol->string (identifier->symbol identifier1))
		  (symbol->string (identifier->symbol identifier2)))))))
  
  (make-comparator identifier?
		   free-identifier=?
		   free-identifier<?
		   #f))

;;; Isolation of identifier access

(define (maybe-isolate isolate? thunk)
  (if isolate?
      (let ((old-used-identifiers #f)
	    (new-used-identifiers (current-used-identifiers)))
	(dynamic-wind
	    (lambda ()
	      (set! old-used-identifiers (current-used-identifiers))
	      (current-used-identifiers new-used-identifiers))
	    thunk
	    (lambda ()
	      (set! new-used-identifiers (current-used-identifiers))
	      (current-used-identifiers old-used-identifiers))))
      (thunk)))

