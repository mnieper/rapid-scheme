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

;; TODO: Syntax-check primitive procedures

(define (cps-transform exp)
  (parameterize
      ((current-reference-method
	transform-atomic-expression)
       (current-literal-method
	transform-atomic-expression)
       (current-undefined-method
	transform-atomic-expression)
       (current-procedure-call-method
	(lambda (expression k flags marks)
	  (cond
	   ((primitive (procedure-call-operator expression))
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
		   (else
		    (transform-primitive-call expression k flags marks)))))
	   (else
	    (transform-procedure-call expression k flags marks)))))
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
    (transform exp
	       (lambda (a) a)
	       #f
	       (lambda ()
		 (expression '() exp)))))

(define (transform expression k flag marks)
  (expression-dispatch expression k flag marks))

(define (transform-atomic-expression exp k flag marks)
  (if (ignore-value? k)
      ((ignore-value-procedure k) #f)
      ((continuation-procedure k) exp)))

(define (transform-procedure-call exp k flag marks)
  (transform* (cons (procedure-call-operator exp)
		    (procedure-call-operands exp))
	      (lambda (t*)
		(expression (,(car t*)
			     ,(continuation-expression k)
			     ,(flag-expression flag)
			     ,(marks)
			     ,@(cdr t*))
			    exp))
	      #f marks))

(define (transform-primitive-call exp k flag marks)
  (transform* (procedure-call-operands exp)
	      (lambda (t*)
		((continuation-procedure k)
		 (expression (,(procedure-call-operator exp)
			      ,@t*)
			     exp)))
	      #f marks))

(define (wcm mark result k flag marks)
  (let ((m* (make-location #f)))
    (expression
     (,(generate-procedure/body
	(list m*)
	'()
	(list (result k #t (lambda () (make-reference m* #f)))))
      ,(flag-marks flag mark marks))
     #f)))

(define (transform-ccm expression k flag marks)
  ((continuation-procedure k)
   (marks)))

(define (transform-wcm expression k flag marks)
  (let*
      ((operands (procedure-call-operands expression))
       (mark (car operands))
       (result (cadr operands)))
    (transform mark
	       (lambda (m)
		 (wcm m
		      (lambda (k flag marks)
			(transform result k flag marks))
		      k flag marks))
	       #f marks)))

(define (transform-apply exp k flag marks)
  (let ((operands (procedure-call-operands exp)))
    (cond
     ((primitive (car operands))
      (transform* (cdr operands)
		  (lambda (a*)
		    ((continuation-procedure k)
		     (expression
		      (,(procedure-call-operator exp)
		       ,(car operands)
		       ,@a*)
		      exp)))
		  #f marks))
     (else      
      (transform* operands
		  (lambda (a*)
		    (expression
		     (,(procedure-call-operator exp)
		      ,(car a*)
		      ,(continuation-expression k)
		      ,(flag-expression flag)
		      ,(marks)
		      ,@(cdr a*))
		     exp))
		  #f marks)))))

(define (transform-assignment exp k flag marks)
  (transform (assignment-expression exp)
	     (lambda (t)
	       ((continuation-procedure k)
		(expression
		 (set! (assignment-location exp)
		       ,t))))
	     #f marks))

(define (transform-multiple-assignment exp k flag marks)
  (let*
      ((formals (multiple-assignment-formals exp))
       (new-formals
	(make-formals
	 (map (lambda (argument) (make-location #f)) (formals-fixed formals))
	 (map (lambda (argument) (make-location #f)) (formals-rest formals))
	 #f)))
    (transform (multiple-assignment-expression exp)
	       (generate-procedure/clause
		(make-clause
		 new-formals
		 (append
		  (map
		   (lambda (argument new-argument)
		     (expression
		      (set! argument
			    ,(make-reference new-argument #f))
		      #f))
		   (formals-fixed formals)
		   (formals-fixed new-formals))
		  (if (not (null? (formals-rest formals)))
		      (list
		       (expression
			(set! (car (formals-rest formals))
			      ,(make-reference
				(car (formals-rest new-formals))
				#f))
			#f))
		      '())
		  (list
		   (expression
		    (,(continuation-expression k)
		     ,(list (make-undefined #f)))
		    #f)))))
	       #f marks)))

(define (transform-call/cc exp k flag marks)
  (transform (car (procedure-call-operands exp))
	     (lambda (a)
	       (continuation-location
		k
		(lambda (c)
		  (expression
		   (,a
		    c
		    ,(flag-expression flag)
		    ,(marks)
		    ,(let ((%c (make-location #f))
			   (%f (make-location #f))
			   (%m* (make-location #f))
			   (x* (make-location #f)))
		       (generate-procedure/body
			(list %c %f %m*) (list x*)
			(list
			 (make-procedure-call
			  (make-reference
			   (make-primitive 'apply #f)
			   #f)
			  (list
			   (make-reference c #f)
			   (make-reference x* #f))
			  #f)))))))))
	     #f marks))

(define (transform-procedure exp k flag marks)
  (do-transform* transform-procedure-clause
		 (procedure-clauses exp)
		 (lambda (c*)
		   ((continuation-procedure k)
		    ;; XXX: Check whether this introduces a non-labeled procedure.
		    (make-procedure c* #f)))
		 flag marks))

(define (transform-procedure-clause clause k flag marks)
  (with-syntax (clause-syntax clause)
    (let*
	((formals (clause-formals clause))
	 (c (make-location #f))
	 (f (make-location #f))
	 (m* (make-location #f)))
      ((continuation-procedure k)
       (make-clause (make-formals
		     (append (list c f m*) (formals-fixed formals))
		     (formals-rest formals)
		     (formals-syntax formals))
		    (transform-body (clause-body clause)
				    (make-reference c #f)
				    (make-reference f #f) ; FIXME (What ???)
				    (lambda () (make-reference m* #f)))
		    #f)))))

(define (transform-sequence exp k flag marks)
  (make-sequence
   (transform-body (sequence-expressions exp) k flag marks)
   #f))

(define (transform-conditional exp k flag marks)
  (continuation-location
   k
   (lambda (c)
     (transform (conditional-test exp)
		(lambda (a)
		  (make-conditional a
				    (transform (conditional-consequent exp)
					       (make-reference c #f)
					       flag marks)
				    (transform (conditional-alternate exp)
					       (make-reference c #f)
					       flag marks)
				    #f))
		#f marks))))

(define (transform-letrec-expression exp k flag marks)
  (do-transform* transform-letrec-definition
		 (letrec-expression-definitions exp)
		 (lambda (b*)
		   (make-letrec-expression
		    b*
		    (transform-body (letrec-expression-body exp) k flag marks)
		    #f))
		 #f marks))

(define (transform-letrec-definition variables k flag marks)
  (with-syntax (variables-syntax variables)
    (transform-procedure (variables-expression variables)
			 (lambda (t)
			   ((continuation-procedure k)
			    (make-variables (variables-formals variables)
					    t
					    #f)))
			 flag marks)))

(define (transform-let-values-expression exp k flag marks)
  (let ((variables (let-values-expression-definition exp)))
    (transform (variables-expression variables)
	       (generate-procedure/clause
		(make-clause
		 (variables-formals variables)
		 (transform-body (let-values-expression-body exp)
				 k
				 flag
				 marks)
		 #f))
	       #f marks)))

(define (transform-body body k flag marks)
  (let-values
      ((body
	(let loop ((body body))
	  (let ((e (car body)) (e* (cdr body)))
	    (transform e
		       (if (null? e*)
			   k
			   (make-ignore-value
			    (lambda (a)
			      (let-values ((a* (loop e*)))
				(if a
				    (apply values a a*)
				    (apply values a*))))))
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

(define generate-procedure/body
  (case-lambda
   ((fixed-parameters rest-parameter* body)
    (generate-procedure/body fixed-parameters rest-parameter* body
			     (lambda (c) (make-reference c #f))))
   ((fixed-parameters rest-parameter* body c)
    (generate-procedure/clause
     (make-clause (make-formals fixed-parameters rest-parameter* #f) body #f)
     c))))

(define generate-procedure/clause
  (case-lambda
   ((clause)
    (generate-procedure/clause clause (lambda (c) (make-reference c #f))))
   ((clause c)
    (let ((f (make-location #f)))
      (make-letrec-expression
       (list
	(make-variables (make-formals (list f) #f)
			(make-procedure (list clause) #f)
			#f))
       (list (c f))
       #f)))))

(define (continuation-procedure k)
  (cond
   ((procedure? k)
    k)
   ((ignore-value? k)
    (ignore-value-procedure k))
   (else
    (lambda (e)
      (expression (,k ,e))))))

(define (continuation-expression k)
  (cond
   ((procedure? k)
    (let ((v (make-location #f)))
      (let-values ((body (k (make-reference v #f))))
	(generate-procedure/body (list v) '() body))))
   ((ignore-value? k)
    (let ((v (make-location #f)))
      (let-values ((body ((ignore-value-procedure k) #f)))
	(generate-procedure/body '() (list v) body))))
   (else
    k)))

(define (continuation-location k c)
  (cond
   ((procedure? k)
    (let ((v (make-location #f)))
      (let-values ((body (k (make-reference v #f))))
	(generate-procedure/body (list v) '() body c))))
   ((ignore-value? k)
    (let ((v (make-location #f)))
      (let-values ((body ((ignore-value-procedure k) #f)))
	(generate-procedure/body '() (list v) body c))))
   (else
    (let ((v (make-location #f)))
      (expression
       (,(generate-procedure/body (list v) '()
				  (list
				   (c v)))
	,k))))))

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
	 (generate-procedure/body
	  (list m)
	  '()
	  (list
	   (make-conditional flag (consequent reference) (alternate reference) #f)))
	 (list mark)
	 #f))
      (let ((mark (lambda () mark)))
	(if flag (consequent mark) (alternate mark)))))

(define (primitive expression)
  (and-let* (((reference? expression))
	     (location (reference-location expression))
	     ((primitive? location)))
    (primitive-value location)))

;; TODO: Add other types of continuations here.
(define-record-type <continuation>
  #f
  continuation?)

(define-record-type (<ignore-value> <continuation>)
  (make-ignore-value procedure)
  ignore-value?
  (procedure ignore-value-procedure))
