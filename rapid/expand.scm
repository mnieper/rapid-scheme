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

(define current-context (make-parameter #f))
(define (top-level-context?) (eq? (current-context) 'top-level))
(define (definition-context?) (eq? (current-context) 'definition))
(define (expression-context?) (eq? (current-context) 'expression))

(define (make-definition formals expression syntax)
  (vector formals expression syntax))
(define (definition-formals definition)
  (vector-ref definition 0))
(define (definition-expression definition)
  (vector-ref definition 1))
(define (definition-syntax definition)
  (vector-ref definition 2))

(define current-definitions (make-parameter #f))

(define (add-definition! formals expression syntax)
  (list-queue-add-back! (current-definitions)
			(make-definition formals expression syntax)))

(define current-expressions (make-parameter #f))

(define expand-into-expression-hook (make-parameter #f))
(define (expand-into-expression expression)
  ((expand-into-expression-hook) expression))

(define (expand-top-level! syntax*)
  (parameterize
      ((current-context 'top-level)
       (current-definitions (list-queue))
       (expand-into-expression-hook
	(lambda (expression)
	  (add-definition! (make-dummy-formals)
			   expression
			   #f))))
    (for-each expand-syntax! syntax*)
    (current-context 'expression)
    (list-queue-map!
     (lambda (definition)
       (let ((expression (definition-expression definition)))       
	 (make-variables (definition-formals definition)
			 (if (syntax? expression)
			     (expand-expression expression) ;; FIXME: This may return
			                                    ;; false
			     expression)
			 (definition-syntax definition))))
     (current-definitions))))

(define (expand-body syntax*)
  (error "expand-body: not implemented yet"))

(define (expand-expression syntax)
  (call-with-current-continuation
   (lambda (return)
     (parameterize ((current-context 'expression)
		    (expand-into-expression-hook return))
       (expand-syntax! syntax)))))

(define (expand-into-syntax-definition identifier-syntax transformer syntax)
  (and-let*
      (((or (not (expression-context?))
	    (begin (raise-syntax-error syntax
				       "unexpected syntax definition of ‘~a’"
				       (syntax->datum identifier-syntax))
		   #f)))
       ((or (not (current-expressions))
	    (begin (raise-syntax-error syntax
				       "syntax definition of ‘~a’ may not follow "
				       "expressions in a body"
				       (syntax->datum identifier-syntax))))))
    (insert-syntactic-binding! identifier-syntax transformer)))

;;; XXX: Some transformers may expand into ‘expand-into-expression’.
;;; How will this be handled?

(define (expand-syntax! syntax)
  (maybe-isolate (top-level-context?)
    (lambda ()
      (let ((form (unwrap-syntax syntax)))
	(cond
	 ((simple-datum? form)
	  (expand-into-expression (make-literal form syntax)))
	 ((null? form)
	  (raise-syntax-error syntax "empty application in source"))
	 ((identifier? form)
	  #f) ;; FIXME
         ((list? form)
	  (cond
	   ((lookup-transformer! (car form))
	    => (lambda (transformer)
		 ((transformer-proc transformer) syntax)))
	   (else
	    #f ;; TODO
	    )))
	 (else
	  (raise-syntax-error syntax "invalid form")))))))

(define (lookup-transformer! syntax)
  (and-let*
      ((form (unwrap-syntax syntax))
       ((identifier? form))
       (denotation (lookup-denotation! syntax))
       ((transformer? denotation)))
    denotation))

(define (simple-datum? expression)
  (or (number? expression)
      (boolean? expression)
      (char? expression)
      (string? expression)
      (bytevector? expression)
      (vector? expression)))

(define (make-dummy-formals)
  (make-formals (list (make-location #f)) #f))
