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

(define-record-type <expression>
  #f
  expression?
  (syntax expression-syntax))

;; References

(define-record-type (<reference> <expression>)
  (make-reference location syntax)
  reference?
  (location reference-location))

;; Primitive references

(define-record-type (<primitive-reference> <expression>)
  (make-primitive-reference primitive syntax)
  primitive-reference?
  (primitive primitive-reference-primitive))

;; Literals

(define-record-type (<literal> <expression>)
  (make-literal datum syntax)
  literal?
  (datum literal-datum))

;; Procedure calls

(define-record-type (<procedure-call> <expression>)
  (make-procedure-call operator operands syntax)
  procedure-call?
  (operator procedure-call-operator)
  (operands procedure-call-operands))

;; Sequences

(define-record-type (<sequence> <expression>)
  (%make-sequence expressions syntax)
  sequence?
  (expressions sequence-expressions))

(define (make-sequence expressions syntax)
  (let ((expression* (flatten expressions)))
    (if (= (length expression*) 1)
	(car expression*)
	(%make-sequence expression* syntax))))

;; Assignment

(define-record-type (<assignment> <expression>)
  (make-assignment location expression syntax)
  assignment?
  (location assignment-location)
  (expression assignment-expression))


;;; Conditionals

(define-record-type (<conditional> <expression>)
  (make-conditional test consequent alternate syntax)
  conditional?
  (test conditional-test)
  (consequent conditional-consequent)
  (alternate conditional-alternate))

;;; Undefined

(define-record-type (<undefined> <expression>)
  (make-undefined syntax)
  undefined?)

;;; Extra types

(define-record-type <variables>
  (make-variables formals expression syntax)
  variables?
  (formals variables-formals)
  (expression variables-expression)
  (syntax variables-syntax))

;; Formals

(define-record-type <formals>
  (%make-formals fixed rest syntax)
  formals?
  (fixed formals-fixed)
  (rest formals-rest)
  (syntax formals-syntax))

(define make-formals
  (case-lambda
   ((fixed syntax)
    (%make-formals fixed #f syntax))
   ((fixed rest syntax)
    (%make-formals fixed '() rest))))

;;; Utility functions

(define (flatten expression*)
  (apply append (map
		 (lambda (expression)
		   (if (sequence? expression)
		       (sequence-expressions expression)
		       (list expression)))
		 expression*)))

