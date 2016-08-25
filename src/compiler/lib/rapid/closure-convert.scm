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
		    (label (make-location variable #f)))
	       (init-var! variable)
	       (var-set-well-known-flag! variable #t)
	       (var-set-label! variable label)
	       (procedure-set-label! procedure label)
	       (uncover-known-calls! procedure)))
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
    '())
  (define constants (make-constant-set))
  (define (add-constant! var lit)
    (set! constants (cons (cons var lit) constants)))
  (define (map-constants proc)
    (map (lambda (constant) (proc (car constant) (cdr constant))) constants))
  (define (constant-definitions)
    (map-constants
     (lambda (var lit)
       (make-variables
	(make-formals (list var) '() (location-syntax var))
	lit
	(location-syntax var)))))

  ;; Procedures
  (define (make-procedure-set)
    '())
  (define procedures (make-procedure-set))
  (define (add-procedure! label procedure)
    (set! procedures (cons (cons label procedure) procedures)))
  (define (map-procedures proc)
    (map (lambda (procedure) (proc (car procedure) (cdr procedure))) procedures))
  (define (procedure-definitions)
    (map-procedures
     (lambda (label procedure)
       (make-variables
	(make-formals (list label) '() (location-syntax label))
	procedure
	(location-syntax label)))))
  
  ;; Environment
  (define (make-environment)
    (imap var-comparator))
  (define env (make-environment))
  (define (save-env) env)
  (define (restore-env! old-env)
    (set! env old-env))
  (define (closure-var? var)
    (let ((value (imap-ref/default env var #f)))
      (and var (eq? 'variable (car value)))))
  (define (make-var-alias var alias)
    (cons 'alias
	  (lambda (syntax)
	    (make-reference alias syntax))))
  (define (make-var-const const)
    (cons 'constant
	  (lambda (syntax)
	    (make-reference const syntax))))
  (define (make-var-arg arg)
    (cons 'variable
	  (lambda (syntax)
	    (make-reference arg syntax))))
  (define current-closure-arg (make-parameter #f))
  (define (make-var-closure index)
    (cons 'variable
	  (lambda (syntax)
	    (make-procedure-call (make-reference (make-primitive 'closure-ref syntax)
						 syntax)
				 (make-reference (current-closure-arg) syntax)
				 (make-literal index syntax)
				 syntax))))
  (define (var->exp var)
    ((cdr (imap-ref env var)) (location-syntax var)))
  (define (map-var-const! var const)
    (set! env (imap-replace env var (make-var-const const))))
  (define (var->closure var)
    (and-let*
	((value (imap-ref/default env var #f))
	 (thunk (cdr value)))
      (thunk (location-syntax var))))
  (define (map-var-arg! var arg)
    (set! env (imap-replace env var (make-var-arg arg))))
  (define (map-var-closure! var index)
    (set! env (imap-replace env var (make-var-closure index))))
  
  (define (convert exp)
    (expression-dispatch exp))

  (define (convert* exp)
    (expression-dispatch* exp))

  (parameterize
      ((current-reference-method
	(lambda (exp)
	  (let ((location (reference-location exp)))
	    (if (primitive? location)
		exp
		(var->exp location)))))
       (current-literal-method
	(lambda (exp)
	  (let ((var (make-location #f)))
	    (add-constant! var exp)
	    (map-var-const! var var)
	    (make-reference var #f))))
       (current-procedure-call-method
	;; TODO: Handle apply.
	(lambda (exp)
	  (let*
	      ((label (procedure-call-label exp))
	       (operator (procedure-call-operator exp))
	       (operands (convert* (procedure-call-operands exp))))
	    (if label
		(let ((closure (var->closure (reference-location operator))))
		  (if closure
		      (make-procedure-call label
					   (cons closure operands)
					   #f)
		      (make-procedure-call label
					   operands
					   #f)))
		(make-procedure-call (convert operator) operands #f)))))
       (current-procedure-method
	(lambda (exp)
	  ;; TODO: Add procedure to label table;
	  ;; FIXME
	  (for-each
	   (lambda (clause)
	     (for-each
	      (lambda (var)
		(map-var-arg! var var))
	      (formals-locations (clause-formals clause))))
	   (procedure-clauses exp))	
	  ;; TODO: Add closure parameters for not-well-known procedures
	  exp
	  ))
       (current-letrec-expression-method
	(lambda (exp)
	  (let*
	      ((old-env (save-env))
	       (definitions
		(letrec-expression-definitions exp))
	       (variables
		(map (lambda (definition)
		       (formals-location (variables-formals definition)))
		     definitions))
	       (procedures
		(map variables-expression definitions))
	       (well-known
		(every procedure-well-known-flag procedures))
	       (free-vars
		(free-var-union (map procedure-free-vars procedures)))
	       (free-vars
		(free-var-delete* free-vars variables))
	       (closure-vars
		(imap-fold
		 (lambda (var _ closure-vars)
		   (cond
		    ((closure-var? var)
		     (cons var closure-vars))
		    (else
		     closure-vars)))
		 '() free-vars)))
	    (for-each
	     (lambda (var)
	       (if well-known
		   (if (= 1 (length closure-vars))
		       (map-var-alias! var (car closure-vars))
		       (unless (null? closure-vars)
			 (map-var-arg! var)))
					; NOT WELL_KNOWN
		   ))
	     variables)
	    
	    ;; TODO: Map! variables bound by this letrec-expression.
	    ;; This should haben before the environment is saved above!
	    ;; FIXME/TODO
	    (restore-env! env) ; Has to happen before the body is evaluated!
	    exp
	    ))))
    (let*
	((exp (convert exp)))
      (make-letrec-expression
       ;; TODO: Use list-queues
       (append (constant-definitions) ; XXX: Not all literals are lifted.
	                              ; Which should we lift?
	       (procedure-definitions))
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
   ((identifier)
    (imap var-comparator identifier #t))))

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

(define (var-label location)
  (vector-ref (denotation-aux location) 1))

(define (var-set-well-known-flag! location flag)
  (vector-set! (denotation-aux location) 0 flag))

(define (var-set-label! location label)
  (vector-set! (denotation-aux location) 1 label))

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
