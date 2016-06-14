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

;;; Denotations

(define-record-type <denotation>
  #f
  denotation?
  (value denotation-value)
  (syntax denotation-syntax))

(define-record-type (<primitive> <denotation>)
  (make-primitive value syntax)
  primitive?)

;; TODO: Add denotation for transformers
;; TODO: Add denotation for locations

;;; Syntactic bindings

(define current-references (make-parameter '()))

(define-record-type <syntactic-binding>
  (%make-syntactic-binding syntax denotation scope reference-count)
  syntactic-binding?
  (syntax binding-syntax)
  (denotation binding-denotation)
  (scope binding-scope)
  (reference-count binding-reference-count binding-set-reference-count!))

(define (make-syntactic-binding syntax denotation)
  (%make-syntactic-binding syntax denotation current-syntactic-environment 0))

(define (increment-reference-count! binding)
  (binding-set-reference-count! binding
				(+ (binding-reference-count binding) 1)))

(define (decrement-reference-count! binding)
  (binding-set-reference-count! binding
				(- (binding-reference-count binding) 1)))

(define (reference-binding! binding)
  (current-references (cons binding (current-references)))
  (increment-reference-count! binding))

(define (binding-referenced? binding)
  (and (eq? (binding-scope binding) (current-syntactic-environment))
       (> (binding-reference-count binding) 0)))

(define (identifier-referenced? identifier)
  (and-let*
      ((binding (imap-ref/default (current-bindings) identifier #f))
       ((binding-referenced? binding)))
    binding))
	 
;;; Syntactic environments

;; bindings is a map identifier->syntactic-binding
(define-record-type <syntactic-environment>
  (%make-syntactic-environment bindings)
  syntactic-environment?
  (bindings syntactic-environment-bindings
	    syntactic-environment-set-bindings!))

(define (make-syntactic-environment)
  (%make-syntactic-environment (imap identifier-comparator)))

;; imports will be a map mapping all identifiers at once covariantly

;; exports is a map identifier-to-be-exported->export-spec

(define (export-syntactic-environment! environment exports)
  (imap-for-each
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
  (cond
   ((imap-ref/default (current-bindings) (unwrap-syntax identifier-syntax) #f)
    => (lambda (binding)
	 (reference-binding! binding)
	 binding))
   (else
    (raise-syntax-error identifier-syntax
			"identifier ‘~a’ not bound"
			(syntax->datum identifier-syntax))
    #f)))

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
     ((identifier-referenced? identifier-syntax)
      => (lambda (binding)
	   (raise-syntax-error identifier-syntax
			       "meaning of identifier ‘~a’ cannot be changed"
			       (syntax->datum identifier-syntax))
	   (raise-syntax-note (binding-syntax binding)
			      "identifier ‘~a’ was bound here"
			      (syntax->datum identifier-syntax))
	   #f))
     (else
      (let ((binding (make-syntactic-binding identifier-syntax)))
	(current-bindings (imap-replace (current-bindings) identifier binding))
	(reference-binding! binding))))))

(define (syntactic-environment-insert-binding! syntactic-environment
					       identifier-syntax
					       denotation)
  (with-syntactic-environment syntactic-environment
    (insert-syntactic-binding! identifier-syntax denotation)))

(define current-syntactic-environment (make-parameter #f))

(define current-bindings
  (case-lambda
   (() (syntactic-environment-bindings (current-syntactic-environment)))
   ((bindings)
    (syntactic-environment-set-bindings! (current-syntactic-environment)
					 bindings))))

(define-syntax with-syntactic-environment
  (syntax-rules ()
    ((with-syntactic-environment syntactic-environment
       body1 body2 ...)
     (parameterize ((current-syntactic-environment
		     syntactic-environment))
       body1 body2 ...))))
