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

(define-syntax define-syntactic-environment
  (syntax-rules ()
    ((define-syntactic-environment environment
       (define-transformer (name syntax)
	 body1 body2 ...)
       ...)
     (begin
       (define environment (make-syntactic-environment))
       (with-syntactic-environment environment
	 (insert-syntactic-binding!
	  (derive-syntax (symbol->identifier 'name) #f)
	  (make-transformer (lambda (syntax)
			      body1 body2 ...)
			    #f))
	 ...)))))

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
  (make-syntactic-binding binding-syntax denotation)
  syntactic-binding?
  (binding-syntax binding-syntax)
  (denotation binding-denotation))

;;; Denotations

;; Variable bindings

(define-record-type <location>
  (make-location syntax)
  location?
  (syntax location-syntax))

(define-record-type <primitive>
  (make-primitive value syntax)
  primitive?
  (value primitive-value)
  (syntax primitive-syntax))

(define-record-type <transformer>
  (make-transformer proc syntax)
  transformer?
  (proc transformer-proc)
  (syntax transformer-syntax))

;;; Syntactic environments

;; bindings is a map identifier->syntactic-binding
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
			       ;; TODO: Use an iset when it is implemented.
			       (imap identifier-comparator)))

(define (export-syntactic-environment! environment exports)
  ;; FIXME: This is far too slow.
  (exports-for-each
   (lambda (identifier export-spec)
     (and-let*
	 ((denotation
	   (syntactic-environment-lookup-denotation! environment
						     (export-spec-source
						      export-spec))))
       (insert-syntactic-binding! (export-spec-target export-spec) denotation)))
   exports))

(define (import-syntactic-environment! environment imports)
  (export-syntactic-environment! environment
				 (imports (syntactic-environment-bindings
					   environment))))

(define (lookup-syntactic-binding! identifier-syntax)
  (let ((identifier (unwrap-syntax identifier-syntax)))
     (let loop ((environments (cons (current-syntactic-environment)
				    (identifier-closure identifier))))
       (cond
	((null? environments)
	 (raise-syntax-error identifier-syntax
			     "identifier ‘~a’ not bound"
			     (identifier->symbol identifier))
	 #f)
	((imap-ref/default (syntactic-environment-bindings (car environments))
			   identifier
			   #f)
	 => (lambda (binding)
	      (use-identifier! identifier binding)
	      binding))
	(else
	 (loop (cdr environments)))))))

(define (lookup-denotation! identifier-syntax)
  (and-let*
      ((binding (lookup-syntactic-binding! identifier-syntax)))
    (binding-denotation binding)))

(define (syntactic-environment-lookup-denotation! environment identifier-syntax)
  (with-syntactic-environment environment
    (lookup-denotation! identifier-syntax)))

(define (insert-syntactic-binding! identifier-syntax denotation)
  (let ((identifier (unwrap-syntax identifier-syntax)))
    (cond
     ((imap-ref/default (current-used-identifiers)
			(unwrap-syntax identifier-syntax)
			#f)
      => (lambda (binding)
	   (raise-syntax-error identifier-syntax
			       "meaning of identifier ‘~a’ cannot be changed"
			       (syntax->datum identifier-syntax))
	   (raise-syntax-note (binding-syntax binding)
			      "identifier ‘~a’ was bound here"
			      (syntax->datum identifier-syntax))
	   #f))
     (else
      (let ((binding
	     (make-syntactic-binding identifier-syntax denotation)))	     
	(use-identifier! (unwrap-syntax identifier-syntax) binding)
	(current-bindings (imap-replace (current-bindings)
					identifier
					binding)))))))

(define (syntactic-environment-insert-binding! syntactic-environment
					       identifier-syntax
					       denotation)
  (with-syntactic-environment syntactic-environment
    (insert-syntactic-binding! identifier-syntax denotation)))

(define (identifier=? environment1 identifier1 environment2 identifier2)
  (let ((denotation1
	 (syntactic-environment-lookup-denotation! environment1 identifier1))
	(denotation2
	 (syntactic-environment-lookup-denotation! environment2 identifier2)))
    (cond
     ((and denotation1 denotation2)
      (eq? denotation1 denotation2))
     ((and (not denotation1) (not denotation2))
      (symbol=? (identifier->symbol identifier1)
		(identifier->symbol identifier2)))
     (else
      #f))))
     
(define (free-identifier=? identifier1 identifier2)
  (identifier=? (current-syntactic-environment) identifier1
		(current-syntactic-environment) identifier2))

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

