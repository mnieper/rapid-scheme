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

(define-library (rapid expressions)
  (export expression? expression-syntax
	  expression-aux expression-set-aux!
	  make-reference reference? reference-location
	  make-primitive-reference primitive-reference? primitive-reference-primitive
	  make-literal literal? literal-datum
	  make-procedure-call procedure-call?
	  procedure-call-operator procedure-call-operands
	  make-sequence sequence? sequence-expressions
	  make-assignment assignment? assignment-location assignment-expression
	  make-conditional conditional?
	  conditional-test conditional-consequent conditional-alternate
	  make-procedure expression-procedure? procedure-clauses
	  make-letrec*-expression letrec*-expression?
	  letrec*-expression-definitions letrec*-expression-body
	  make-undefined undefined?
	  make-variables variables? variables-formals variables-expression
	  variables-syntax
	  make-formals formals? formals-fixed formals-rest formals-syntax
	  formals-locations
	  make-clause clause? clause-formals clause-body clause-syntax
	  expression->datum)
  (import (scheme case-lambda)
	  (rapid base)
	  (rapid and-let)
	  (rapid identifiers)
	  (rapid syntax)
	  (rapid syntactic-environments))
  (include "expressions.scm"))  
