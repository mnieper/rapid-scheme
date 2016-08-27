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
	  (free-var-delete*
	   (free-var-union
	    (cons
	     (uncover-free-variables*! (letrec-expression-body exp))
	     (map
	      (lambda (definition)
		(uncover-free-variables! (variables-expression definition)))
	      (letrec-expression-definitions exp))))
	   (map (lambda (definition)
		  (formals-location (variables-formals
				     definition)))
		(letrec-expression-definitions exp)))))
       (current-sequence-method
	(lambda (exp)
	  (uncover-free-variables*! (sequence-expressions exp))))
       (current-conditional-method
	(lambda (exp)
	  (uncover-free-variables*! (list (conditional-test exp)
					  (conditional-consequent exp)
					  (conditional-alternate exp))))))
    (uncover-free-variables! exp))

  ;; Uncover known calls
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
	;; TODO: Handle apply.
	(lambda (exp)
	  (let ((operator (procedure-call-operator exp)))
	    (uncover-known-calls! operator)
	    (for-each uncover-known-calls! (procedure-call-operands exp))
	    (cond
	     ((and-let*
		  (((reference? operator))
		   (location (reference-location operator))
		   ((not (primitive? location))))
		(var-label location))
	      => (lambda (label)
		   (procedure-call-set-label! exp label)))
	     (else
	      (procedure-call-set-label! exp #f))))))
       (current-procedure-method
	(lambda (exp)
	  (for-each
	   (lambda (clause)
	     (for-each
	      (lambda (variable)
		(init-var! variable)
		(var-set-label! variable #f))
	      (formals-locations (clause-formals clause)))
	     (for-each uncover-known-calls! (clause-body clause)))
	   (procedure-clauses exp))))
       (current-letrec-expression-method
	(lambda (exp)
	  (for-each
	   (lambda (definition)
	     (let* ((procedure (variables-expression definition))
		    (variable (formals-location (variables-formals definition)))
		    (label (make-var #f)))
	       (init-var! variable)
	       (var-set-well-known-flag! variable #t)
	       (var-set-label! variable label)
	       (procedure-set-label! procedure label)))
	   (letrec-expression-definitions exp))
	  (for-each (lambda (definition)
		      (uncover-known-calls! (variables-expression definition)))
		    (letrec-expression-definitions exp))		      
	  (for-each uncover-known-calls! (letrec-expression-body exp))
	  (for-each
	   (lambda (definition)
	     (let ((variable (formals-location (variables-formals definition)))
		   (procedure (variables-expression definition)))
	       (procedure-set-well-known-flag! procedure
					       (var-well-known-flag variable))))
	   (letrec-expression-definitions exp))))
       (current-sequence-method
	(lambda (exp)
	  (for-each uncover-known-calls! (sequence-expressions exp))))
       (current-conditional-method
	(lambda (exp)
	  (uncover-known-calls! (conditional-test exp))
	  (uncover-known-calls! (conditional-consequent exp))
	  (uncover-known-calls! (conditional-alternate exp)))))
    (uncover-known-calls! exp))
  
  ;; Closure conversion
  (convert exp))

;; The actual closure conversion
(define (convert exp)

  ;; Constants
  (define (make-constant-set)
    (list-queue))
  (define constants (make-constant-set))
  (define (add-constant! var lit)
    (list-queue-add-back! constants (cons var lit)))
  (define (map-constants proc)
    (list-queue-map
     (lambda (constant) (proc (car constant) (cdr constant))) constants))
  (define (constant-definitions)
    (map-constants
     (lambda (var lit)
       (make-variables
	(make-formals (list var) '() (location-syntax var))
	lit
	(location-syntax var)))))

  ;; Procedures
  (define (make-procedure-set)
    (list-queue))
  (define procedures (make-procedure-set))
  (define (add-procedure! label procedure)
    (list-queue-add-back! procedures (cons label procedure)))
  (define (map-procedures proc)
    (list-queue-map
     (lambda (procedure) (proc (car procedure) (cdr procedure))) procedures))
  (define (procedure-definitions)
    (map-procedures
     (lambda (label procedure)
       (make-variables
	(make-formals (list label) '() (location-syntax label))
	procedure
	(location-syntax label)))))
    
  (define (convert exp)
    (expression-dispatch exp))

  (define (convert* exp)
    (expression-dispatch* exp))

  (parameterize
      ((current-reference-method
	(lambda (exp)
	  (let ((location (reference-location exp)))
	    (if (primitive? location)
		;; FIXME: Handle references to primitives.
		;; IDEA: Could be lifted as literals
		;; However ... (let ((x car)) ... (apply x ...) <- adds cont to x, etc.
		exp
		(var->ref location exp)))))
       (current-literal-method
	;; TODO: Only lift complicated constants
	;; Or don't lift at all and fold later
	(lambda (exp)
	  (let ((var (make-var #f)))
	    (add-constant! var exp)
	    (var-set-global! var)
	    (make-reference var #f))))
       (current-procedure-call-method
	;; FIXME: Handle apply.
	(lambda (exp)
	  (let*
	      ((operator (procedure-call-operator exp))
	       (operands (convert* (procedure-call-operands exp))))
	    (cond
	     ((and-let*
		  (((reference? operator))
		   (primitive (reference-location operator)))
		(primitive? primitive))
	      (make-procedure-call operator operands #f))
	     (else	  
	      (let ((label (procedure-call-label exp)))
		(if label
		    (let ((code-pointer (reference-location operator)))
		      (if (var-eliminated? code-pointer)
			  (make-procedure-call (make-reference label #f) operands #f)
			  (make-procedure-call (make-reference label #f)
					       (cons (convert operator) operands) #f)))
		    (make-procedure-call (make-reference (make-primitive 'call #f) #f)
					 (cons (convert operator) operands) #f))))))))
       (current-letrec-expression-method
	(lambda (exp)
	  (let*
	      ((definitions
		 (letrec-expression-definitions exp))
	       (variables
		(map (lambda (definition)
		       (formals-location (variables-formals definition)))
		     definitions))
	       (procedures
		(map variables-expression definitions))
	       (labels
		(map procedure-label procedures))
	       (well-known
		(every procedure-well-known-flag procedures))
	       (free-vars
		(free-var-union (map procedure-free-vars procedures)))
	       (free-vars
		(free-var-delete* free-vars variables))
	       ;; XXX
	       #;(free-vars
		(begin
		  (error (list variables (imap-fold (lambda (key value acc)
						      (cons key acc))
						    '() free-vars)))
		  free-vars))
	       
	       
	       (closure-var-list-queue (list-queue))
	       (cp-count (if well-known 0 (length labels)))
	       (closure-var-count+closure-vars
		(imap-fold
		 (lambda (var _ cnt+vars)
		   (if (and (var-local? var) (not (var-alias? var)))
		       ;; FIXME: Das mit dem Alias ist Unsinn. Es kann nämlich sein,
		       ;; daß die Variable, auf die der Alias zeigt, gar nicht in
		       ;; den Free-Vars ist.		       
		       (let ((i (vector-ref cnt+vars 0)))
			 (list-queue-add-back! closure-var-list-queue var)
			 (vector (+ 1 i)
				 (imap-replace (vector-ref cnt+vars 1)
					       var i)))
		       cnt+vars))
		 (vector cp-count (make-closure-var-set)) free-vars))
	       (closure-var-count (- (vector-ref closure-var-count+closure-vars 0)
				     cp-count))
	       (closure-vars (vector-ref closure-var-count+closure-vars 1))
	       (closure-var-list (list-queue-list closure-var-list-queue))
	       (closure-type
		(if well-known
		    (case closure-var-count
		      ((0) 'eliminated)
		      ((1) 'alias)
		      ((2) 'pair)
		      (else 'vector))
		    (case closure-var-count
		      ((0) 'global)
		      (else 'closure))))
	       (closure-label (make-var #f))
	       (closure-name (make-parameter closure-label)))
	    
	    (case closure-type
	      ((alias)
	       (for-each
		(lambda (variable)
		  (var-set-alias! variable (car closure-var-list))))
	       variables)
	      ((pair vector)
	       (for-each
		(lambda (variable)
		  (var-set-alias! variable closure-label))
		variables))
	      ((global closure)
	       (do ((variables variables (cdr variables))
		    (i 0 (+ 1 i)))
		   ((null? variables))
		 (let ((variable (car variables)))
		   (var-set-code-pointer! variable closure-name i)))))

	    (when (eq? 'global closure-type)
	      (add-constant!
	       closure-label
	       (make-procedure-call
		(make-reference (make-primitive 'make-closure #f) #f)
		(map (lambda (label)
		       (make-reference label #f))
		     labels)
		#f)))

	    (for-each
	     (lambda (label procedure)
	       (let*
		   ((closure-label (make-var (expression-syntax procedure))))
		 (parameterize
		     ((current-closure-vars closure-vars)
		      (current-closure-arg closure-label)
		      (current-closure-type closure-type)
		      (closure-name closure-label))
		   (add-procedure!
		    label
		    (make-procedure
		     (map
		      (lambda (clause)
			(let*
			    ((formals (clause-formals clause)))
			  (for-each
			   (lambda (arg)
			     (var-set-local! arg))
			   (formals-locations formals))
			  (make-clause
			   (make-formals (if (eq? 'eliminated
						  closure-type)
					     (formals-fixed formals)
					     (cons closure-label
						   (formals-fixed formals)))
					 (formals-rest formals)
					 (formals-syntax formals))
			   (convert* (clause-body clause))
			   (clause-syntax clause))))
		      (procedure-clauses procedure))
		     procedure)))))
	     labels procedures)

	    (let*
		((new-exp
		  (make-sequence (convert* (letrec-expression-body exp)) #f))
		 (new-exp
		  (case closure-type
		    ((closure)
		     (make-let-expression
		      (make-variables
		       (make-formals (list closure-label) '() #f)
		       (make-procedure-call
			(make-reference (make-primitive 'make-closure #f) #f)
			(append ; TODO: Optimize
			 (map (lambda (label)
				(make-reference label #f))
			      labels)
			 (map (lambda (var)
				(make-reference (var-alias var) #f))
			      closure-var-list))
			#f)
		       #f)
		      (list new-exp)
		      #f))
		    ((pair)
		     (make-let-expression
		      (make-variables
		       (make-formals (list closure-label) '() #f)
		       (make-procedure-call
			(make-reference (make-primitive 'cons #f) #f)
			(list
			 (make-reference (car closure-var-list) #f)
			 (make-reference (cadr closure-var-list) #f))
			#f)
		       #f)
		      (list new-exp)
		      #f))
		    ((vector)
		     (make-let-expression
		      (make-variables
		       (make-formals (list closure-label) '() #f)
		       (make-procedure-call
			(make-reference (make-primitive 'vector #f) #f)
			(map (lambda (var)
			       (make-reference var #f))
			     closure-var-list)
			#f)
		       #f)
		      (list new-exp)
		      #f))
		    (else
		     new-exp))))
	      new-exp)))))
    (let*
	((exp (convert exp)))
      (make-letrec-expression
       (list-queue-list
	(list-queue-append! (constant-definitions)
			    (procedure-definitions)))
       (list exp)
       exp))))

(define (uncover-free-variables! exp)  
  (expression-dispatch exp))

(define (uncover-known-calls! exp)
  (expression-dispatch exp))

(define (uncover-free-variables*! exp*)
  (free-var-union (expression-dispatch* exp*)))

(define make-free-var-set
  (case-lambda
   (()
    (imap var-comparator))
   ((var)
    (imap var-comparator var #t))))

(define (free-var-union free-var-set*)
  (apply imap-union free-var-set*))

(define (free-var-delete free-var-set identifier)
  (imap-delete free-var-set identifier))

(define (free-var-delete* free-var-set var*)
  (imap-delete-keys free-var-set var*))

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
  (denotation-set-aux! location (vector #t #f 'eliminated location #f)))

(define (make-var syntax)
  (let ((var (make-location syntax)))
    (init-var! var)
    var))

(define (var-well-known-flag location)
  (vector-ref (denotation-aux location) 0))

(define (var-label location)
  (vector-ref (denotation-aux location) 1))

(define (var-type location)
  (vector-ref (denotation-aux location) 2))

(define (var-alias location)
  (vector-ref (denotation-aux location) 3))

(define (var-eliminated? location)
  (eq? 'eliminated (var-type location)))

(define (var-local? location)
  (or
   (eq? 'local (var-type location))
   (eq? 'code-pointer (var-type location))))

(define (var-alias? location)
  (not (eq? location (var-alias location))))

(define (var-set-well-known-flag! location flag)
  (vector-set! (denotation-aux location) 0 flag))

(define (var-set-label! location label)
  (vector-set! (denotation-aux location) 1 label))

(define (var-set-global! location)
  (let ((aux (denotation-aux location)))
    (vector-set! aux 2 'global)))

(define (var-set-local! location)
  (let ((aux (denotation-aux location)))
    (vector-set! aux 2 'local)))

(define (var-set-alias! location alias)
  (let ((aux (denotation-aux location)))
    (vector-set! aux 2 (var-type alias))
    (vector-set! aux 3 alias)))

(define (var-set-code-pointer! var closure-name i)
  (let ((aux (denotation-aux var)))
    (vector-set! aux 2 'code-pointer)
    (vector-set! aux 4 (vector closure-name i))))

(define (var->ref location syntax)
  (let ((alias (var-alias location)))
    (cond
     ((closure-var-index alias)
      => (lambda (index)
	   (make-closure-reference index syntax)))
     ((eq? 'code-pointer (var-type alias))
      (make-code-pointer-reference alias syntax))
     (else
      (make-reference alias syntax)))))

(define current-closure-arg (make-parameter #f))
(define current-closure-type (make-parameter #f))

(define (make-closure-reference index syntax)
  (let ((closure (make-reference (current-closure-arg) syntax)))
    (case (current-closure-type)
      ((alias)
       closure)
      ((pair)
       (make-procedure-call (make-reference (make-primitive (if (zero? index)
								'car
								'cdr)
							    syntax)
					    syntax)
			    (list closure)
			    syntax))
      ((vector)
       (make-procedure-call (make-reference (make-primitive 'vector-ref syntax)
					    syntax)
			    (list closure
				  (make-literal index syntax))
			    syntax))
      ((closure)
       (make-procedure-call (make-reference (make-primitive 'closure-ref syntax)
					    syntax)
			    (list closure
				  (make-literal index syntax))
			    syntax)))))
  
(define (make-code-pointer-reference var syntax)
  (let*
      ((aux (denotation-aux var))
       (aux (vector-ref aux 4)))
    (make-procedure-call (make-reference (make-primitive 'closure->code-pointer syntax) syntax)
			 (list (make-reference ((vector-ref aux 0)) syntax)
			       (make-literal (vector-ref aux 1) syntax))
			 syntax)))

(define (make-closure-var-set) (imap var-comparator))
(define (closure-var-index var)
  (imap-ref/default (current-closure-vars) var #f))

(define (clause-set-free-vars! clause free-vars)
  (clause-set-aux! clause free-vars))

(define (clause-free-vars clause)
  (clause-aux clause))

;;; Constants

(define var-comparator
  (make-comparator location?
		   eq?
		   (lambda (location1 location2)
		     (< (denotation-identity location1)
			(denotation-identity location2)))
		   #f))

;;; Global parameters

(define current-closure-vars (make-parameter (make-closure-var-set)))

