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

;;; TODO: Check whether number of values in continuation and set-values!
;;; is always the correct one.

;; XXX: Check: Does this output non-letrec'ed case-lambda's?

;; TODO: Do not put #f at syntax positions

;; REMOVE SYNTAX STACK (add again later)

(define (cps-transform expression)
  (parameterize
      ((current-reference-method
	transform-atomic-expression)
       (current-literal-method
	transform-atomic-expression)
       (current-undefined-method
	transform-atomic-expression)
       (current-procedure-call-method
	(lambda (expression k flags marks)
	  (let ((operator (procedure-call-operator expression)))
	    ;; also: TODO: handle (apply primitive ...)
	    (cond
	     ((primitive operator)
	      => (lambda (primitive)
		   (case primitive
		     ((call/cc)
		      (transform-call/cc expression k flags marks))
		     ((wcm)
		      (transform-wcm expression k flags marks))
		     ((ccm)
		      (transform-ccm expression k flags marks))
		     ((apply)
		      (transform-apply expression k flags marks))
		     ((error)
		      (transform-error expression k flags marks))
		     (else
		      (transform-primitive-call expression k flags marks)))))
	     (else
	      (transform-procedure-call expression k flags marks))))))
       (current-procedure-method
	transform-procedure)
       (current-assignment-method
	transform-assignment)
       (current-multiple-assignment-method
	transform-multiple-assignment)
       (current-letrec-expression-method
	transform-letrec-expression)
       (current-let-values-expression-method
	transform-let-values-expression)
       (current-sequence-method
	transform-sequence)
       (current-conditional-method
	transform-conditional))
    (transform expression
	       (lambda (a) a)
	       #f
	       (lambda ()
		 (make-literal '() (expression-syntax expression))))))

(define (transform expression k flags marks)
  (expression-dispatch expression k flags marks))

(define (transform-atomic-expression expression k flags marks)
  ((continuation-procedure k) expression))

(define (transform-procedure-call expression k flag marks)
  (define syntax (expression-syntax expression))
  (transform* (cons (procedure-call-operator expression)
		    (procedure-call-operands expression))
	      (lambda (t*)
		(make-procedure-call (car t*)
				     (append (list (continuation-expression k)
						   (flag-expression flag)
						   (marks))
					     (cdr t*))
				     syntax))
	      #f marks))

(define (transform-primitive-call expression k flag marks)
  (transform* (procedure-call-operands expression)
	      (lambda (t*)
		((continuation-procedure k)
		 (make-procedure-call (procedure-call-operator expression)
				      t*
				      (expression-syntax expression))))
	      #f marks))

(define (wcm mark result k flag marks)
  (let ((m* (make-location #f)))
    (make-procedure-call
     (generate-procedure
      (list m*)
      '()
      (list (result k #t (lambda () (make-reference m* #f)))))
     (list
      (flag-marks flag mark marks))
     #f)))

(define (transform-ccm expression k flag marks)
  ((continuation-procedure k)
   (marks)))

(define (transform-wcm expression k flag marks)
  (define operands (procedure-call-operands expression))
  (define mark (car operands))
  (define result (cadr operands))
  (transform mark
	     (lambda (m)
	       (wcm m
		    (lambda (k flag marks)
		      (transform result k flag marks))
		    k flag marks))
	     #f marks))

(define (transform-apply expression k flag marks)
  ;; FIXME: operator may be primitive
  (define operands (procedure-call-operands expression))
  (transform* operands
	      (lambda (a*)
		(make-procedure-call
		 (procedure-call-operator expression)
		 (append
		  (list (car a*) (continuation-expression k) (flag-expression flag) (marks))
		  (cdr a*))
		 (expression-syntax expression)))
	      #f marks))

(define (transform-error expression k flag marks)
  (transform* (procedure-call-operands expression)
	      (lambda (t*)
		((continuation-procedure k)
		 (make-procedure-call (procedure-call-operator expression)
				      (append (list (flag-expression flag) (marks))
					      t*)
				      (expression-syntax expression))))
	      #f marks))

(define (transform-assignment expression k flag marks)
  (transform (assignment-expression expression)
	     (lambda (t)
	       ((continuation-procedure k)
		(make-assignment (assignment-location expression)
				 t
				 (expression-syntax expression))))
	     #f marks))

(define (transform-multiple-assignment expression k flag marks)
  (define formals (multiple-assignment-formals expression))
  (define new-formals
    (make-formals
     (map (lambda (argument) (make-location #f)) (formals-fixed formals))
     (map (lambda (argument) (make-location #f)) (formals-rest formals))
     #f))
  (transform (multiple-assignment-expression expression)
	     (%generate-procedure
	      (make-clause
	       new-formals
	       (append
		(map
		 (lambda (argument new-argument)
		   (make-assignment argument (make-reference new-argument #f) #f))
		 (formals-fixed formals)
		 (formals-fixed new-formals))
		(if (not (null? (formals-rest formals)))
		    (list
		     (make-assignment (car (formals-rest formals))
				      (make-reference
				       (car (formals-rest new-formals))
				       #f)
				      #f))
		    '())
		(list
		 (make-procedure-call (continuation-expression k)
				      (list (make-undefined #f))
				      #f)))
	       #f))
	     #f marks))

(define (transform-call/cc expression k flag marks)
  (transform (car (procedure-call-operands expression))
	     (lambda (a)
	       (let ((c (make-location #f)))
		 (make-procedure-call
		  (generate-procedure
		   (list c)
		   '()
		   (list
		    (make-procedure-call a
					 (list (make-reference c #f)
					       (flag-expression flag)
					       (marks)
					       (let ((%c (make-location #f))
						     (%f (make-location #f))
						     (%m* (make-location #f))
						     (x* (make-location #f)))
						 (generate-procedure
						  (list %c %f %m*) (list x*)
						  (list
						   (make-procedure-call
						    (make-reference
						     (make-primitive 'apply #f)
						     #f)
						    (list
						     (make-reference c #f)
						     (make-reference x* #f))
						    #f)))))
					 (expression-syntax expression))))
		  (list (continuation-expression k))
		  #f)))
	     #f marks))

(define (transform-procedure expression k flag marks)
  (do-transform* transform-procedure-clause
		 (procedure-clauses expression)
		 (lambda (c*)
		   ((continuation-procedure k)
		    (make-procedure c* (expression-syntax expression))))
		 flag marks))

(define (transform-procedure-clause clause k flag marks)
  (define formals (clause-formals clause))
  (let ((c (make-location #f)) (f (make-location #f)) (m* (make-location #f)))
    ((continuation-procedure k)
     (make-clause (make-formals
		   (append (list c f m*) (formals-fixed formals))
		   (formals-rest formals)
		   (formals-syntax formals))
		  (transform-body (clause-body clause)
				  (make-reference c #f)
				  (make-reference f #f) ; FIXME (What ???)
				  (lambda () (make-reference m* #f)))
		  (clause-syntax clause)))))

(define (transform-sequence expression k flag marks)
  (make-sequence
   (transform-body (sequence-expressions expression) k flag marks)
   (expression-syntax expression)))

(define (transform-conditional expression k flag marks)
  (let ((c (make-location #f)))
    (make-procedure-call
     (generate-procedure
      (list c)
      '()
      (list
       (transform (conditional-test expression)
		  (lambda (a)
		    (make-conditional a
				      (transform (conditional-consequent expression)
						 (make-reference c #f)
						 flag marks)
				      (transform (conditional-alternate expression)
						 (make-reference c #f)
						 flag marks)
				      (expression-syntax expression)))
		  #f marks)))
     (list (continuation-expression k))
     #f)))

(define (transform-letrec-expression expression k flag marks)
  (do-transform* transform-letrec-definition
		 (letrec-expression-definitions expression)
		 (lambda (b*)
		   (make-letrec-expression
		    b*
		    (transform-body (letrec-expression-body expression) k flag marks)
		    (expression-syntax expression)))
		 #f marks))

(define (transform-letrec-definition variables k flag marks)
  (transform-procedure (variables-expression variables)
		       (lambda (t)
			 ((continuation-procedure k)
			  (make-variables (variables-formals variables)
					  t
					  (variables-syntax variables))))
		       flag marks))

(define (transform-let-values-expression expression k flag marks)
  (define variables (let-values-expression-definition expression))
  (transform (variables-expression variables)
	     (%generate-procedure
	      (make-clause
	       (variables-formals variables)
	       (transform-body (let-values-expression-body expression) k flag marks)
	       (expression-syntax expression)))
	     #f marks))

(define (transform-body body k flag marks)
  (let-values
      ((body
	(let loop ((body body))
	  (let ((e (car body)) (e* (cdr body)))
	    (transform e
		       (if (null? e*)
			   k
			   (lambda (a)
			     (let-values ((a* (loop e*)))
			       (if (atomic? a)
				   (apply values a*)
				   (apply values a a*)))))
		       (if (null? e*)
			   flag
			   #f)
		       marks)))))
    body))

(define (do-transform* transformer x* k flag marks)
  (let do-transform* ((x* x*) (k k))
    (if (null? x*)
	((continuation-procedure k) '())
	(transformer (car x*)
		     (lambda (a)
		       (do-transform* (cdr x*)
				      (lambda (a*)
					((continuation-procedure k)
					 (cons a a*)))))
		     flag marks))))

(define (transform* e* k flag marks)
  (do-transform* transform e* k flag marks))

(define (generate-procedure fixed-parameters rest-parameter body)
  (%generate-procedure
   (make-clause (make-formals fixed-parameters rest-parameter #f) body #f)))

(define (%generate-procedure clause)
  (let ((f (make-location #f)))
    (make-letrec-expression
     (list
      (make-variables (make-formals (list f) #f)
		      (make-procedure (list clause) #f)
		      #f))
     (list (make-reference f #f))
     #f)))

(define (continuation-procedure k)
  (if (procedure? k)
      k
      (lambda e* (make-procedure-call k e* #f))))

(define (continuation-expression k)
  (if (procedure? k)
      (let ((c (make-location #f)))
	(let-values ((body (k (make-reference c #f))))
	  (generate-procedure (list c) '() body)))
      k))

(define (flag-expression flag)
  (if (expression? flag)
      flag
      (make-literal flag #f)))

(define (flag-marks flag mark marks)
  (define (consequent mark)
    (make-procedure-call (make-reference (make-primitive 'cons #f) #f)
			 (list (mark)
			       (make-procedure-call (make-reference
						     (make-primitive 'cdr #f)
						     #f)
						    (list (marks))
						    #f))
			 #f))
  (define (alternate mark)
    (make-procedure-call (make-reference (make-primitive 'cons #f) #f)
			 (list (mark) (marks))
			 #f))
  (if (expression? flag)
      (let* ((m (make-location #f)))
	(define (reference) (make-reference m #f))
	(make-procedure-call
	 (generate-procedure
	  (list m)
	  '()
	  (list
	   (make-conditional flag (consequent reference) (alternate reference) #f)))
	 (list mark)
	 #f))
      (let ((mark (lambda () mark)))
	(if flag (consequent mark) (alternate mark)))))

(define (atomic? expression)
  (or (reference? expression)
      (literal? expression)
      (undefined? expression)))

(define (primitive expression)
  (and-let* (((reference? expression))
	     (location (reference-location expression))
	     ((primitive? location)))
    (primitive-value location)))
