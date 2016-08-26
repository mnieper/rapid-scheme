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

;;; Syntax

(define-syntax with-syntax
  (syntax-rules ()
    ((with-syntax syntax . body)
     (parameterize ((current-syntax (convert/syntax syntax))) . body))))

(define-syntax expression
  (syntax-rules ()
    ((expression e)
     (expression e #f))
    ((expression e syntax)
     (let-syntax ((k
		   (syntax-rules ()
		     ((k v)
		      v))))
       (let ((s (if (expression? syntax)
		    (expression-syntax syntax)
		    syntax)))
	 (expression-helper k e s))))))

(define-syntax expression-helper
  (syntax-rules (quote unquote quasiquote set! if case-lambda)
    ((expression-helper k ,e _)
     (k e))
    ((expression-helper k 'c syntax)
     (let ((v (make-literal 'c syntax)))
       (k v)))
    ((expression-helper k `q syntax)
     (let ((v (make-literal `q syntax)))
       (k v)))    
    ((expression-helper k (set! var e) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k v)
		      (let ((v1 (make-assignment var v syntax)))
			(k v1))))))
       (expression-helper k1 e syntax)))
    ((expression-helper k (if e e1) syntax)
     (expression-helper k (if e e1 ,(make-undefined syntax)) syntax))
    ((expression-helper k (if e e1 e2) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k1 v v1 v2)
		      (let ((v3 (make-conditional v v1 v2 syntax)))
			(k v3))))))
       (expression-helper* k1 (e e1 e2) syntax)))
    ((expression-helper k (case-lambda . c*) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k1 l)
		      (let ((v (make-procedure l syntax)))
			(k v))))))
       (expression-clauses k1 c* syntax)))
    ((expression-helper k (operator operand ...) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k1 v . v*)
		      (let ((v1 (make-procedure-call `v `v* syntax)))
			(k v1))))))
       (expression-template k1 (operator operand ...) syntax)))
    ((expression-helper k e syntax)
     (let ((v (make-reference e syntax)))
       (k v)))))

(define-syntax expression-helper*
  (syntax-rules ()
    ((expression-helper* k e* syntax)
     (expression-helper* k e* () syntax))
    ((expression-helper* k () v* syntax)
     (k . v*))
    ((expression-helper* k (e . e*) (v ...) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k1 v1)
		      (expression-helper* k e* (v ... v1) syntax)))))
       (expression-helper k1 e syntax)))))

(define-syntax expression-template
  (syntax-rules (unquote-splicing)
    ((expression-template k e* syntax)
     (expression-template k e* () syntax))
    ((expression-template k () v* syntax)
     (k . v*))
    ((expression-template k (,@e . e*) (v ...) syntax)
     (expression-template k e* (v ... ,@e) syntax))
    ((expression-template k (e . e*) (v ...) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k1 v1)
		      (expression-template k e* (v ... ,v1) syntax)))))
       (expression-helper k1 e syntax)))))

(define-syntax expression-clauses
  (syntax-rules (unquote-splicing)
    ((expression-clauses k () syntax)
     (let ((v (list)))
       (k v)))
    ((expression-clauses k (,@e . c*) syntax)
     (let-syntax ((k1
		   (syntax-rules ()
		     ((k1 l)
		      (k (append e l))))))
       (expression-clauses k1 c* syntax)))))
  
;;; Expressions

(define-record-type <expression>
  #f
  expression?
  (syntax expression-syntax)
  (method current-expression-method)
  (aux expression-aux expression-set-aux!))

(define (convert/syntax value)
  (cond
   ((syntax? value)
    value)
   ((expression? value)
    (expression-syntax value))
   ((not value)
    (current-syntax))
   (else
    (error "invalid syntax value" value))))

(define (expression-dispatch exp . args)
  (with-syntax exp
    (apply ((current-expression-method exp)) exp args)))

(define (expression-dispatch* expression* . args)
  (map (lambda (expression)
	 (apply expression-dispatch expression args))
       expression*))

;; References

(define current-reference-method
  (make-parameter
   (lambda (expression . args)
     expression)))

(define-record-type (<reference> <expression>)
  (%make-reference location syntax method)
  reference?
  (location reference-location))

(define (make-reference location syntax)
  (%make-reference location (convert/syntax syntax) current-reference-method))

;; Literals

(define current-literal-method
  (make-parameter
   (lambda (expression . args)
     expression)))

(define-record-type (<literal> <expression>)
  (%make-literal datum syntax method)
  literal?
  (datum literal-datum))

(define (make-literal datum syntax)
  (%make-literal datum (convert/syntax syntax) current-literal-method))

;; Undefined

(define current-undefined-method
  (make-parameter
   (lambda (expression . args)
     expression)))

(define-record-type (<undefined> <expression>)
  ;; Current Larceny compiler produces an error on (%make-undefined syntax method). 
  (%make-undefined method syntax)
  undefined?)

(define (make-undefined syntax)
  (%make-undefined current-undefined-method (convert/syntax syntax)))

;; Procedure calls

(define current-procedure-call-method
  (make-parameter
   (lambda (expression . args)
     (make-procedure-call (apply expression-dispatch
				 (procedure-call-operator expression)
				 args)
			  (apply expression-dispatch*
				 (procedure-call-operands expression)
				 args)
			  (expression-syntax expression)))))

(define-record-type (<procedure-call> <expression>)
  (%make-procedure-call operator operands syntax method)
  procedure-call?
  (operator procedure-call-operator)
  (operands procedure-call-operands))

(define (make-procedure-call operator operands syntax)
  (and operator
       (every (lambda (x) x)
	      operands)
       (%make-procedure-call operator operands (convert/syntax syntax)
			     current-procedure-call-method)))

;; Sequences

(define current-sequence-method
  (make-parameter
   (lambda (expression . args)
     (make-sequence (apply expression-dispatch* (sequence-expressions expression)
			   args)
		    (expression-syntax expression)))))

(define-record-type (<sequence> <expression>)
  (%make-sequence expressions syntax method)
  sequence?
  (expressions sequence-expressions))

(define (make-sequence expressions syntax)
  (let ((expression* (flatten expressions)))
    (and (not (null? expression*))
	 (if (= (length expression*) 1)
	     (car expression*)
	     (%make-sequence expression*
			     (convert/syntax syntax)
			     current-sequence-method)))))

;; Assignment

(define current-assignment-method
  (make-parameter
   (lambda (expression . args)
     (make-assignment (assignment-location expression)
		      (apply expression-dispatch
			     (assignment-expression expression)
			     args)
		      (expression-syntax expression)))))

(define-record-type (<assignment> <expression>)
  (%make-assignment location expression syntax method)
  assignment?
  (location assignment-location)
  (expression assignment-expression))

(define (make-assignment location expression syntax)
  (and expression
       (%make-assignment location
			 expression
			 (convert/syntax syntax)
			 current-assignment-method)))

;; Multiple assignment

(define current-multiple-assignment-method
  (make-parameter
   (lambda (expression . args)
     (make-multiple-assignment (multiple-assignment-formals expression)
			       (apply expression-dispatch
				      (assignment-expression expression)
				      args)
			       (expression-syntax expression)))))

(define-record-type (<multiple-assignment> <expression>)
  (%make-multiple-assignment formals expression syntax method)
  multiple-assignment?
  (formals multiple-assignment-formals)
  (expression multiple-assignment-expression))

(define (make-multiple-assignment formals expression syntax)
  (let ((location (formals-location formals)))
    (if location
	(make-assignment location expression (convert/syntax syntax))  
	(and expression
	     (%make-multiple-assignment formals expression (convert/syntax syntax)
					current-multiple-assignment-method)))))

;; Conditionals

(define current-conditional-method
  (make-parameter
   (lambda (expression . args)
     (make-conditional (apply expression-dispatch
			      (conditional-test expression)
			      args)
		       (apply expression-dispatch
			      (conditional-consequent expression)
			      args)
		       (apply expression-dispatch
			      (conditional-alternate expression)
			      args)
		       (expression-syntax expression)))))

(define-record-type (<conditional> <expression>)
  (%make-conditional test consequent alternate syntax method)
  conditional?
  (test conditional-test)
  (consequent conditional-consequent)
  (alternate conditional-alternate))

(define (make-conditional test consequent alternate syntax)
  (and test consequent alternate
       (%make-conditional test consequent alternate (convert/syntax syntax)
			  current-conditional-method)))

;; Procedures

(define current-procedure-method
  (make-parameter
   (lambda (expression . args)
     (make-procedure (map (lambda (clause)
			    (make-clause (clause-formals clause)
					 (apply expression-dispatch*
						(clause-body clause)
						args)
					 (clause-syntax clause)))
			  (procedure-clauses expression))
		     (expression-syntax expression)))))
			  
(define-record-type (<procedure> <expression>)
  (%make-procedure clauses syntax method)
  expression-procedure?
  (clauses procedure-clauses))

(define (make-procedure clauses syntax)
  (%make-procedure clauses (convert/syntax syntax) current-procedure-method))

;; Letrec* expressions

(define current-letrec*-expression-method
  (make-parameter
   (lambda (expression . args)
     (make-letrec*-expression (map (lambda (variables)
				     (make-variables (variables-formals variables)
						     (apply expression-dispatch
							    (variables-expression
							     variables)
							    args)
						     (variables-syntax variables)))
				   (letrec*-expression-definitions expression))
			      (apply expression-dispatch*
				     (letrec*-expression-body expression)
				     args)
			      (expression-syntax expression)))))

(define-record-type (<letrec*-expression> <expression>)
  (%make-letrec*-expression definitions body syntax method)
  letrec*-expression?
  (definitions letrec*-expression-definitions)
  (body letrec*-expression-body))

(define (make-letrec*-expression definitions body syntax)
  ;; XXX: The body may be flattened.
  (%make-letrec*-expression definitions body (convert/syntax syntax)
			    current-letrec*-expression-method))

;; Letrec expressions

(define current-letrec-expression-method
  (make-parameter
   (lambda (expression . args)
     (make-letrec-expression (map (lambda (variables)
				    (make-variables (variables-formals variables)
						    (apply expression-dispatch
							   (variables-expression
							    variables)
							   args)
						    (variables-syntax variables)))
				   (letrec-expression-definitions expression))
			     (apply expression-dispatch*
				    (letrec-expression-body expression)
				    args)
			     (expression-syntax expression)))))

(define-record-type (<letrec-expression> <expression>)
  (%make-letrec-expression definitions body syntax method)
  letrec-expression?
  (definitions letrec-expression-definitions)
  (body letrec-expression-body))

(define (make-letrec-expression definitions body syntax)
  (if (null? definitions)
      (make-sequence body (convert/syntax syntax))   
      (%make-letrec-expression definitions
			       (flatten body)
			       (convert/syntax syntax)
			       current-letrec-expression-method)))

;; Let-values expression

(define current-let-values-expression-method
  (make-parameter
   (lambda (expression . args)
     (make-let-values-expression (let ((variables (let-values-expression-definition
						   expression)))
				   (make-variables (variables-formals variables)
						   (apply expression-dispatch
							  (variables-expression
							   variables)
							  args)
						   (variables-syntax variables)))
				 (apply expression-dispatch*
					(let-values-expression-body expression)
					args)
				 (expression-syntax expression)))))

(define-record-type (<let-values-expression> <expression>)
  (%make-let-values-expression definition body syntax method)
  let-values-expression?
  (definition let-values-expression-definition)
  (body let-values-expression-body))

(define (make-let-values-expression definition body syntax)
  (%make-let-values-expression definition (flatten body) (convert/syntax syntax)
			       current-let-values-expression-method))

;; Let expression

(define current-let-expression-method
  (make-parameter
   (lambda (expression . args)
     (make-let-expression (let ((variables (let-expression-definition
					    expression)))
			    (make-variables (variables-formals variables)
					    (apply expression-dispatch
						   (variables-expression
						    variables)
						   args)
					    (variables-syntax variables)))
			  (apply expression-dispatch*
				 (let-values-body expression)
				 args)
			  (expression-syntax expression)))))

(define-record-type (<let-expression> <expression>)
  (%make-let-expression definition body syntax method)
  let-expression?
  (definition let-expression-definition)
  (body let-expression-body))

(define (make-let-expression definition body syntax)
  (%make-let-expression definition (flatten body) (convert/syntax syntax)
			current-let-expression-method))


;; Extra types

(define-record-type <variables>
  (%make-variables formals expression syntax aux)
  variables?
  (formals variables-formals)
  (expression variables-expression)
  (syntax variables-syntax)
  (aux variables-aux variables-set-aux!))

(define (make-variables formals expression syntax)
  (%make-variables formals expression (convert/syntax syntax) #f))

;; Formals

(define-record-type <formals>
  (%make-formals fixed rest* syntax)
  formals?
  (fixed formals-fixed)
  (rest* formals-rest)
  (syntax formals-syntax))

(define (formals-locations formals)
  (append (formals-fixed formals) (formals-rest formals)))

(define (formals-location formals)
  (and (= (length (formals-fixed formals)) 1)
       (null? (formals-rest formals))
       (car (formals-fixed formals))))

(define make-formals
  (case-lambda
   ((fixed syntax)
    (%make-formals fixed '() (convert/syntax syntax)))
   ((fixed rest* syntax)
    (%make-formals fixed rest* (convert/syntax syntax)))))

(define-record-type <clause>
  (%make-clause formals body syntax)
  clause?
  (formals clause-formals)
  (body clause-body)
  (syntax clause-syntax)
  (aux clause-aux clause-set-aux!))

(define (make-clause formals body syntax)
  (%make-clause formals body (convert/syntax syntax)))

;;; Utility functions

(define (flatten expression*)
  (apply append (map
		 (lambda (expression)
		   (cond
		    ((sequence? expression)
		     (sequence-expressions expression))
		    (expression
		     (list expression))
		    (else
		     '())))
		 expression*)))

;;; Expression datums

(define (expression->datum expression)
  ;; TODO: Make use of expression-dispatch in this procedure.
  (cond
   ;; References
   ((reference? expression)
    (let ((location (reference-location expression)))
      (if (primitive? location)
	  (primitive-value location)
	  (location->symbol location))))

   ;; Literals
   ((literal? expression)
    (let ((datum (literal-datum expression)))
      (if (self-evaluating? datum)
	  datum
	  `(quote ,datum))))

   ;; Undefined
   ((undefined? expression)
    `(if #f #f))

   ;; Procedure calls
   ((procedure-call? expression)
    `(,(expression->datum (procedure-call-operator expression))
      ,@(map expression->datum (procedure-call-operands expression))))

   ;; Procedures 
   ((expression-procedure? expression)
    `(case-lambda
      ,@(map
	 (lambda (clause)
	   `(,(formals->datum (clause-formals clause))
	     ,@(map expression->datum (clause-body clause))))
	 (procedure-clauses expression))))

   ;; Assignments
   ((assignment? expression)
    `(set! ,(location->symbol (assignment-location expression))
	   ,(expression->datum (assignment-expression expression))))

   ;; Multiple assignments
   ((multiple-assignment? expression)
    `(set!-values ,(formals->datum (multiple-assignment-formals expression))
		 ,(expression->datum (multiple-assignment-expression expression))))
   
   ;; Letrec* expressions
   ((letrec*-expression? expression)
    `(letrec*-values
      ,(map
	(lambda (variables)
	  `(,(formals->datum (variables-formals variables))
	    ,(expression->datum (variables-expression variables))))
	(letrec*-expression-definitions expression))
      ,@(map expression->datum (letrec*-expression-body expression))))

   ;; Letrec expression
   ((letrec-expression? expression)
    `(letrec
	 ,(map
	   (lambda (variables)
	     `(,(car (formals->datum (variables-formals variables)))
	       ,(expression->datum (variables-expression variables))))
	   (letrec-expression-definitions expression))
       ,@(map expression->datum (letrec-expression-body expression))))

   ;; Let-values expression
   ((let-values-expression? expression)
    `(let-values
	 (,(let ((variables (let-values-expression-definition expression)))
	     `(,(formals->datum (variables-formals variables))
	       ,(expression->datum (variables-expression variables)))))
       ,@(map expression->datum (let-values-expression-body expression))))

   ;; Let-values expression
   ((let-expression? expression)
    `(let
	 (,(let ((variables (let-expression-definition expression)))
	     `(,(formals->datum (variables-formals variables))
	       ,(expression->datum (variables-expression variables)))))
       ,@(map expression->datum (let-expression-body expression))))
   
   ;; Sequences
   ((sequence? expression)
    `(begin ,@(map expression->datum (sequence-expressions expression))))
	       
   ;; Conditionals
   ((conditional? expression)
    `(if ,(expression->datum (conditional-test expression))
	 ,(expression->datum (conditional-consequent expression))
	 ,(expression->datum (conditional-alternate expression))))
   
   (else
    (error "bad expression" expression))))

(define (location->symbol location)
  (let ((string (string-append "g"
			       (number->string (denotation-identity location)))))
    (string->symbol
     (or (and-let*
	     ((syntax (location-syntax location))
	      (form (unwrap-syntax (location-syntax location)))
	      ((identifier? form)))
	   (string-append string "_"
			  (symbol->string (identifier->symbol form))))
	 string))))

(define (formals->datum formals)
  (let loop ((fixed (formals-fixed formals)))
    (if (null? fixed)
	(let ((rest* (formals-rest formals)))
	  (if (null? rest*)
	      '()
	      (location->symbol (car rest*))))
	(cons (location->symbol (car fixed))
	      (loop (cdr fixed))))))

(define (self-evaluating? datum)
  (or (number? datum)
      (string? datum)
      (char? datum)
      (vector? datum)
      (bytevector? datum)
      (boolean? datum)))
