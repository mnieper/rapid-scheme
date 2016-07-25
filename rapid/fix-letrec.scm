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

(define (set-variables! location variables)
  (denotation-set-aux! location variables))

(define (lookup-variables location)
  (denotation-aux location))

(define (set-current-free-reference-adder! binding-construct current-adder)
  (expression-set-aux! binding-construct current-adder))

(define (add-free-reference! location)
  (and-let*
      ((variables (lookup-variables location))
       (binding-construct (lookup-binding-construct variables)))
    (((expression-aux binding-construct)) location)))

(define (make-variables-datum binding-construct identity)
  (vector binding-construct #f #f #f '() #f #f identity))

(define (lookup-binding-construct variables)
  (vector-ref (variables-aux variables) 0))

(define (variables-complex? variables)
  (vector-ref (variables-aux variables) 1))

(define (set-complex?! variables complex)
  (vector-set! (variables-aux variables) 1 complex))

(define (free-references variables)
  (vector-ref (variables-aux variables) 2))

(define (set-free-references! variables free-references)
  (vector-set! (variables-aux variables) 2 free-references))

(define (transformed-init variables)
  (vector-ref (variables-aux variables) 3))

(define (set-transformed-init! variables transformed-init)
  (vector-set! (variables-aux variables) 3 transformed-init))

(define (add-dependency! variables dependent-variables)
  (let ((vector (variables-aux variables)))
    (vector-set! vector 4 (cons dependent-variables
				(vector-ref vector 4)))
    (when (eq? variables dependent-variables)
      (vector-set! vector 6 #t))))

(define (dependency-list variables)
  (vector-ref (variables-aux variables) 4))

(define (referenced? variables)
  (vector-ref (variables-aux variables) 5))

(define (reference! variables)
  (vector-set! (variables-aux variables) 5 #t))

(define (self-dependent? variables)
  (vector-ref (variables-aux variables) 6))

(define definition-comparator
  (make-comparator variables?
		   eq?
		   (lambda (variables1 variables2)
		     (< (vector-ref (variables-aux variables1) 7)
			(vector-ref (variables-aux variables2) 7)))
		   #f))

;; TODO: Add a (pre- or post-) pass that implements the restrictions
;; of the letrec and the letrec* constructs in 4.2.2 of the report.

(define (fix-letrec expression)
  
  (define current-complex! (make-parameter (lambda () #f)))

  (define (complex!) ((current-complex!)))

  (define (fix-letrec*-expression expression)

    (define definition-identity 0)

    (define (generate-definition-identity)
      (let ((identity definition-identity))
	(set! definition-identity (+ 1 identity))
	identity))
				   
    (define (init-variables! variables)
      (variables-set-aux! variables
			  (make-variables-datum expression
						(generate-definition-identity))))
    
    (define definitions (letrec*-expression-definitions expression))

    (define current-free-reference-adder (make-parameter #f))

    (define (lambda-definition? variables)
      (expression-procedure? (transformed-init variables)))

    (define (make-definitions variables thunk)
      (let loop ((locations (formals-locations (variables-formals variables))))
	(if (null? locations)
	    (thunk)
	    (list
	     (make-let-values-expression
	      (make-variables
	       (make-formals (list (car locations)) '())
	       (make-undefined (location-syntax (car locations)))
	       (location-syntax (car locations)))
	      (loop (cdr locations))
	      (location-syntax (car locations)))))))

    (define (make-assignments variables thunk)
      (cons (make-multiple-assignment
	     (variables-formals variables)
	     (transformed-init variables)
	     (variables-syntax variables))
	    (thunk)))

    ;; Initialize current binding construct
    (set-current-free-reference-adder! expression current-free-reference-adder)
    
    ;; Record definitions and locations for each variables definition
    (for-each
     (lambda (variables)
       (init-variables! variables)
       (for-each
	(lambda (location)
	  (set-variables! location variables))
	(formals-locations (variables-formals variables))))
     definitions)
    
    ;; Transform inits and record free references and complexity
    (for-each
     (lambda (variables)
       (let*
	   ((complex #f)
	    (free-references '())
	    (transformed-init
	     (parameterize ((current-complex!
			     (lambda ()
			       (set! complex #t)))
			    (current-free-reference-adder
			     (lambda (location)
			       (set! free-references
				     (cons location free-references)))))
	       (fix-letrec (variables-expression variables)))))
	 (set-complex?! variables complex)
	 (set-free-references! variables free-references)
	 (set-transformed-init! variables transformed-init)))
     definitions)
    
    ;; Record dependencies
    (let ((last-complex-variables #f))
      (for-each
       (lambda (variables)
	 (for-each
	  (lambda (referenced-location)
	    (let ((referenced-variables (lookup-variables referenced-location)))
	      (add-dependency! referenced-variables variables)
	      (reference! referenced-variables)))
	  (free-references variables))
	 (when (variables-complex? variables)
	   (when last-complex-variables
	     (add-dependency! last-complex-variables variables))
	   (set! last-complex-variables variables)))
       definitions))

    (let*
	((transformed-body
	  (parameterize ((current-free-reference-adder
			  (lambda (location)
			    (reference! (lookup-variables location)))))
	    (map-in-order fix-letrec (letrec*-expression-body expression))))
	 (sccs
	  ;; Build dependency graph
	  (graph-scc
	   (map
	    (lambda (variables)
	      (cons variables (dependency-list variables)))
	    definitions)
	   definition-comparator)))
      
      ;; Construct transformed expression.
      (make-sequence
       (let loop ((sccs sccs))
	 (let ((rest
		(lambda ()
		  (loop (cdr sccs)))))
	   (if
	    (null? sccs)
	    ;; Finally inject the body.
	    transformed-body
	    ;; Transform next scc of bindings.
	    (let ((scc (car sccs)))
	      (if (= (length scc) 1)
		  ;; Single bindings
		  (let ((variables (car scc)))
		    (cond
		     ;; Single procedure binding
		     ((lambda-definition? variables)
		      (if (referenced? variables)
			  (list
			   (make-letrec-expression		    
			    (list
			     (make-variables
			      (variables-formals variables)
			      (transformed-init variables)
			      (variables-syntax variables)))
			    (rest)
			    (variables-syntax variables)))
			  (rest)))
		     ;; Single simple binding
		     ((not (self-dependent? variables))
		      (cond
		       ((referenced? variables)
			(list
			 (make-let-values-expression
			  (make-variables
			   (variables-formals variables)
			   (transformed-init variables)
			   (variables-syntax variables))
			  (rest)
			  (variables-syntax variables))))
		       ((variables-complex? variables)
			(cons (transformed-init variables)
			      (rest)))
		       (else
			(rest))))
		     ;; Single complex binding
		     (else
		      (make-definitions
		       variables
		       (lambda ()
			 (make-assignments variables rest))))))
		  ;; Multiple bindings
		  (let loop ((variables* scc))
		    (if (null? variables*)
			(list
			 (make-letrec-expression
			  (let loop ((variables* scc))
			    (if (null? variables*)
				'()
				(let ((variables (car variables*)))
				  (if (lambda-definition? variables)
				      (cons
				       (make-variables
					(variables-formals variables)
					(transformed-init variables)
					(variables-syntax variables))
				       (loop (cdr variables*)))
				      (loop (cdr variables*))))))
			  (let loop ((variables* scc))
			    (if (null? variables*)
				(rest)
				(cond
				 ((lambda-definition? (car variables*))
				  (loop (cdr variables*)))
				 ((referenced? (car variables*))
				  (make-assignments (car variables*)
						    (lambda ()
						      (loop (cdr variables*)))))
				 (else
				  (cons
				   (transformed-init (car variables*))
				   (loop (cdr variables*)))))))
			  (expression-syntax expression)))
			(let ((variables (car variables*)))
			  (if (or (lambda-definition? variables)
				  (not (referenced? variables)))
			      (loop (cdr variables*))
			      (make-definitions (car variables*)
						(lambda ()
						  (loop (cdr variables*)))))))))))))
       (expression-syntax expression))))
  
  (define (fix-letrec expression)
    (expression-dispatch expression))
  
  (parameterize
      ((current-reference-method
	(lambda (expression)
	  (let ((location (reference-location expression)))
	    (unless (primitive? location)
	      (add-free-reference! location))
	    expression)))
       (current-literal-method
	(lambda (expression)
	  expression))
       (current-undefined-method
	(lambda (expression)
	  expression))
       (current-sequence-method
	(lambda (expression)
	  (make-sequence (map fix-letrec (sequence-expressions expression))
			 (expression-syntax expression))))
       (current-conditional-method
	(lambda (expression)
	  (make-conditional (fix-letrec (conditional-test expression))
			    (fix-letrec (conditional-consequent expression))
			    (fix-letrec (conditional-alternate expression))
			    (expression-syntax expression))))
       (current-procedure-call-method
	(lambda (expression)
	  (complex!)
	  (make-procedure-call (fix-letrec (procedure-call-operator expression))
			       (map fix-letrec (procedure-call-operands expression))
			       (expression-syntax expression))))
       (current-procedure-method
	(lambda (expression)
	  (parameterize ((current-complex!
			  (lambda () #f)))
	    (let ((clauses (procedure-clauses expression)))
	      (for-each (lambda (clause)
			  (for-each
			   (lambda (location)
			     (set-variables! location #f))
			   (formals-locations (clause-formals clause))))
			clauses)
	      (make-procedure
	       (map (lambda (clause)
		      (make-clause
		       (clause-formals clause)
		       (map fix-letrec (clause-body clause))
		       (clause-syntax clause)))
		    clauses)
	       (expression-syntax expression))))))
       (current-assignment-method
	(lambda (expression)
	  (complex!)
	  (add-free-reference! (assignment-location expression))
	  (make-assignment (assignment-location expression)
			   (fix-letrec (assignment-expression expression))
			   (expression-syntax expression))))
       (current-letrec*-expression-method
	fix-letrec*-expression))

    (fix-letrec expression)))
