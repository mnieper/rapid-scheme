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

;;; Expressions

(define (default-method expression . args)
  (error "unhandled expression type" expression))

(define-record-type <expression>
  #f
  expression?
  (syntax expression-syntax)
  (method current-expression-method)
  (aux expression-aux expression-set-aux!))

(define (expression-dispatch expression . args)
  (apply ((current-expression-method expression)) expression args))

;; References

(define current-reference-method (make-parameter default-method))

(define-record-type (<reference> <expression>)
  (%make-reference location syntax method)
  reference?
  (location reference-location))

(define (make-reference location syntax)
  (%make-reference location syntax current-reference-method))

;; Primitive references

(define current-primitive-reference-method (make-parameter default-method))

(define-record-type (<primitive-reference> <expression>)
  (%make-primitive-reference primitive syntax method)
  primitive-reference?
  (primitive primitive-reference-primitive))

(define (make-primitive-reference primitive syntax)
  (%make-primitive-reference primitive syntax current-primitive-reference-method))

;; Literals

(define current-literal-method (make-parameter default-method))

(define-record-type (<literal> <expression>)
  (%make-literal datum syntax method)
  literal?
  (datum literal-datum))

(define (make-literal datum syntax)
  (%make-literal datum syntax current-literal-method))

;; Procedure calls

(define current-procedure-call-method (make-parameter default-method))

(define-record-type (<procedure-call> <expression>)
  (%make-procedure-call operator operands syntax method)
  procedure-call?
  (operator procedure-call-operator)
  (operands procedure-call-operands))

(define (make-procedure-call operator operands syntax)
  (%make-procedure-call operator operands syntax current-procedure-call-method))

;; Sequences

(define current-sequence-method (make-parameter default-method))

(define-record-type (<sequence> <expression>)
  (%make-sequence expressions syntax method)
  sequence?
  (expressions sequence-expressions))

(define (make-sequence expressions syntax)
  (let ((expression* (flatten expressions)))
    (if (= (length expression*) 1)
	(car expression*)
	(%make-sequence expression* syntax current-sequence-method))))

;; Assignment

(define current-assignment-method (make-parameter default-method))

(define-record-type (<assignment> <expression>)
  (%make-assignment location expression syntax method)
  assignment?
  (location assignment-location)
  (expression assignment-expression))

(define (make-assignment location expression syntax)
  (%make-assignment location expression syntax current-assignment-method))

;; Multiple assignment

(define current-multiple-assignment-method (make-parameter default-method))

(define-record-type (<multiple-assignment> <expression>)
  (%make-multiple-assignment formals expression syntax method)
  multiple-assignment?
  (formals multiple-assignment-formals)
  (expression multiple-assignment-expression))

(define (make-multiple-assignment formals expression syntax)
  (%make-multiple-assignment formals expression syntax current-multiple-assignment-method))

;; Conditionals

(define current-conditional-method (make-parameter default-method))

(define-record-type (<conditional> <expression>)
  (%make-conditional test consequent alternate syntax method)
  conditional?
  (test conditional-test)
  (consequent conditional-consequent)
  (alternate conditional-alternate))

(define (make-conditional test consequent alternate syntax)
  (%make-conditional test consequent alternate syntax
		     current-conditional-method))

;; Undefined

(define current-undefined-method (make-parameter default-method))

(define-record-type (<undefined> <expression>)
  ;; Current Larceny compiler produces an error on (%make-undefined syntax method). 
  (%make-undefined method syntax)
  undefined?)

(define (make-undefined syntax)
  (%make-undefined current-undefined-method syntax))

;; Procedures

(define current-procedure-method (make-parameter default-method))

(define-record-type (<procedure> <expression>)
  (%make-procedure clauses syntax method)
  expression-procedure?
  (clauses procedure-clauses))

(define (make-procedure clauses syntax)
  (%make-procedure clauses syntax current-procedure-method))

;; Letrec* expressions

(define current-letrec*-expression-method (make-parameter default-method))

(define-record-type (<letrec*-expression> <expression>)
  (%make-letrec*-expression definitions body syntax method)
  letrec*-expression?
  (definitions letrec*-expression-definitions)
  (body letrec*-expression-body))

(define (make-letrec*-expression definitions body syntax)
  ;; XXX: The body may be flattened.
  (%make-letrec*-expression definitions body syntax
			    current-letrec*-expression-method))

;; Letrec expressions

(define current-letrec-expression-method (make-parameter default-method))

(define-record-type (<letrec-expression> <expression>)
  (%make-letrec-expression definitions body syntax method)
  letrec-expression?
  (definitions letrec-expression-definitions)
  (body letrec-expression-body))

(define (make-letrec-expression definitions body syntax)
  (if (null? definitions)
      (make-sequence body syntax)   
      (%make-letrec-expression definitions (flatten body) syntax
			       current-letrec-expression-method)))

;; Let-values expression

(define current-let-values-expression-method (make-parameter default-method))

(define-record-type (<let-values-expression> <expression>)
  (%make-let-values-expression definition body syntax method)
  let-values-expression?
  (definition let-values-expression-definition)
  (body let-values-expression-body))

(define (make-let-values-expression definition body syntax)
  (%make-let-values-expression definition (flatten body) syntax
			       current-let-values-expression-method))


;; Extra types

(define-record-type <variables>
  (%make-variables formals expression syntax aux)
  variables?
  (formals variables-formals)
  (expression variables-expression)
  (syntax variables-syntax)
  (aux variables-aux variables-set-aux!))

(define (make-variables formals expression syntax)
  (%make-variables formals expression syntax #f))

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
    (%make-formals fixed '() syntax))
   ((fixed rest* syntax)
    (%make-formals fixed rest* syntax))))

(define-record-type <clause>
  (make-clause formals body syntax)
  clause?
  (formals clause-formals)
  (body clause-body)
  (syntax clause-syntax))

;;; Utility functions

(define (flatten expression*)
  (apply append (map
		 (lambda (expression)
		   (if (sequence? expression)
		       (sequence-expressions expression)
		       (list expression)))
		 expression*)))

;;; Expression datums

(define (expression->datum expression)
  ;; TODO: Make use of expression-dispatch in this procedure.
  (cond
   ;; References
   ((reference? expression)
    (location->symbol (reference-location expression)))

   ;; Primitive reference
   ((primitive-reference? expression)
    (primitive-value (primitive-reference-primitive expression)))

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
         
   ;; Sequences
   ((sequence? expression)
    `(begin ,@(map expression->datum (sequence-expressions expression))))
	       
   ;; Conditionals
   ((conditional? expression)
    `(if ,(expression->datum (conditional-test expression))
	 ,(expression->datum (conditional-consequent expression))
	 ,(expression->datum (conditional-alternate expression))))
   
   (else
    `BAD ;; XXX
    #;(error "bad expression" expression))))

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
