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

(define (closure-convert exp)

  ;; TODO: Lift letrec expressions. => extra pass with procedural cont's.
  
  ;; Uncover free variables
  (parameterize
      ((current-reference-method
	(lambda (exp)
	  (let ((location (reference-location exp)))
	    (if (primitive? location)
		(make-free-var-set)
		(make-free-var-set location)))))
       (current-literal-method
	(lambda (exp)
	  (make-free-var-set)))
       (current-undefined-method
	(lambda (exp)
	  (make-free-var-set)))
       (current-procedure-call-method
	(lambda (exp)
	  (uncover-free-variables*! (cons (procedure-call-operator exp)
					  (procedure-call-operands exp)))))
       (current-procedure-method
	(lambda (exp)
	  (let*
	      ((free-vars
		(free-var-union
		 (map
		  (lambda (clause)
		    (let*
			((free-vars
			  (uncover-free-variables*! (clause-body clause)))
			 (free-vars (free-var-delete* free-vars
						      (formals-locations 
						       (clause-formals clause)))))
		      (clause-set-free-vars! clause free-vars)
		      free-vars))
		  (procedure-clauses exp)))))
	    (procedure-set-free-vars! exp free-vars)
	    free-vars)))
       (current-letrec-expression-method
	(lambda (exp)
	  (free-var-union
	   (cons
	    (uncover-free-variables*! (letrec-expression-body exp))
	    (map
	     (lambda (definition)
	       (free-var-delete (uncover-free-variables! (variables-expression
							  definition))
				(formals-location (variables-formals
						   definition))))
	     (letrec-expression-definitions exp))))))
       (current-sequence-method
	(lambda (exp)
	  (uncover-free-variables*! (sequence-expressions exp))))
       (current-conditional-method
	(lambda (exp)
	  (uncover-free-variables*! (list (conditional-test exp)
					  (conditional-consequent exp)
					  (conditional-alternate exp))))))
    (uncover-free-variables! exp))

  (parameterize
      ((current-reference-method
	(lambda (exp)
	  (let ((location (reference-location exp)))
	    (unless (primitive? location)
	      (var-set-well-known-flag! location #f)))))
       (current-literal-method
	(lambda (exp)
	  #f))
       (current-undefined-method
	(lambda (exp)
	  #f))
       (current-procedure-call-method
	(lambda (exp)
	  (let ((operator (procedure-call-operator exp)))
	    (uncover-known-calls! operator)
	    (for-each uncover-known-calls! (procedure-call-operands exp))
	    (cond
	     ((and-let*
		  (((reference? operator))
		   (location (reference-location operator))
		   ((not (primitive? location)))
		   ((var-procedure location)))
		location)
	      => (lambda (location)
		   (procedure-call-set-label! exp location)))
	     (else
	      (procedure-call-set-label! exp #f))))))
       (current-procedure-method
	(lambda (exp)
	  (for-each
	   (lambda (clause)
	     (for-each
	      (lambda (variable)
		(init-var! variable)
		(var-set-procedure! variable #f))
	      (formals-locations (clause-formals clause)))
	     (for-each uncover-known-calls! (clause-body clause))
	     (procedure-set-well-known-flag! exp (procedure-label exp)))
	   (procedure-clauses exp))))
       (current-letrec-expression-method
	(lambda (exp)
	  (for-each
	   (lambda (definition)
	     (let* ((procedure (variables-expression definition))
		    (variable (formals-location (variables-formals definition))))
	       (init-var! variable)
	       (var-set-well-known-flag! variable #t)
	       (var-set-procedure! variable procedure)
	       (procedure-set-label! procedure variable)		
	       (uncover-known-calls! procedure)))
	   (letrec-expression-definitions exp))	  
	  (for-each uncover-known-calls! (letrec-expression-body exp))))
       (current-sequence-method
	(lambda (exp)
	  (for-each uncover-known-calls! (sequence-expressions exp))))
       (current-conditional-method
	(lambda (exp)
	  (uncover-known-calls! (conditional-test exp))
	  (uncover-known-calls! (conditional-consequent exp))
	  (uncover-known-calls! (conditional-alternate exp)))))
    (uncover-known-calls! exp))
  
  exp)

(define (uncover-free-variables! exp)  
  (expression-dispatch exp))

(define (uncover-known-calls! exp)
  (expression-dispatch exp))

(define (uncover-free-variables*! exp*)
  (free-var-union (expression-dispatch* exp*)))

(define make-free-var-set
  (case-lambda
   (()
    (imap free-var-comparator))
   ((identifier)
    (imap free-var-comparator identifier #t))))

(define (free-var-union free-var-set*)
  (apply imap-union free-var-set*))

(define (free-var-delete free-var-set identifier)
  (imap-delete free-var-set identifier))

(define (free-var-delete* free-var-set identifier*)
  (imap-delete-keys free-var-set identifier*))

(define (procedure-set-free-vars! procedure free-vars)
  (expression-set-aux! procedure (vector free-vars #f #f)))

(define (procedure-free-vars procedure)
  (vector-ref (expression-aux procedure) 0))

(define (procedure-label procedure)
  (vector-ref (expression-aux procedure) 1))

(define (procedure-set-label! procedure label)
  (vector-set! (expression-aux procedure) 1 label))

(define (procedure-well-known-flag procedure)
  (vector-ref (expression-aux procedure) 2))

(define (procedure-set-well-known-flag! procedure flag)
  (vector-set! (expression-aux procedure) 2 flag))

(define (procedure-call-set-label! exp label)
  (expression-set-aux! exp label))

(define (procedure-call-label exp)
  (expression-aux exp))

(define (init-var! location)
  (denotation-set-aux! location (vector #t #f)))

(define (var-well-known-flag location)
  (vector-ref (denotation-aux location) 0))

(define (var-procedure location)
  (vector-ref (denotation-aux location) 1))

(define (var-set-well-known-flag! location flag)
  (vector-set! (denotation-aux location) 0 flag))

(define (var-set-procedure! location procedure)
  (vector-set! (denotation-aux location) 1 procedure))

(define (clause-set-free-vars! clause free-vars)
  (clause-set-aux! clause free-vars))

(define (clause-free-vars clause)
  (clause-aux clause))

;;; Constants

(define free-var-comparator
  (make-comparator location?
		   eq?
		   (lambda (location1 location2)
		     (< (denotation-identity location1)
			(denotation-identity location2)))
		   #f))
