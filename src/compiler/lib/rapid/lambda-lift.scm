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

(define (lambda-lift exp)
  (parameterize
      ((current-reference-method
	(lambda (exp k)
	  (k exp)))
       (current-literal-method
	(lambda (exp k)
	  (k exp)))
       (current-undefined-method
	(lambda (exp k)
	  (k exp)))
       (current-procedure-call-method
	(lambda (exp k)
	  (apply lift
		 (lambda (e . e*)
		   (k (make-procedure-call e e* exp)))
		 (procedure-call-operator exp)
		 (procedure-call-operands exp))))
       (current-procedure-method
	(lambda (exp k)
	  (k (make-procedure
	      (map
	       (lambda (clause)
		 (make-clause
		  (clause-formals clause)
		  (list
		   (lift (lambda (e) e)
			 (make-sequence (clause-body clause)
					(clause-syntax clause))))
		  (clause-syntax clause)))
	       (procedure-clauses exp))
	      exp))))
       (current-letrec-expression-method
	(lambda (exp k)
	  (k (make-letrec-expression
	      (map
	       (lambda (definition)
		 (make-variables (variables-formals definition)
				 (lift (lambda (e) e)
				       (variables-expression definition))
				 (variables-syntax definition)))
	       (letrec-expression-definitions exp))
	      (list
	       (lift (lambda (e) e)
		     (make-sequence (letrec-expression-body exp)
				    exp)))
	      exp))))
       (current-sequence-method
	(lambda (exp k)
	  (apply lift
		 (lambda e*
		   (k (make-sequence e* exp)))
		 (sequence-expressions exp))))
       (current-conditional-method
	(lambda (exp k)
	  (lift
	   (lambda (e1 e2 e3)
	     (k (make-conditional e1 e2 e3 exp)))
	   (conditional-test exp)
	   (conditional-consequent exp)
	   (conditional-alternate exp)))))
    (lift (lambda (e) e) exp)))

(define (lift k exp . exp*)
  (expression-dispatch exp
		       (lambda (e)
			 (if (null? exp*)
			     (k e)
			     (apply lift
				    (lambda e*
				      (apply k e e*))
				    exp*)))))
