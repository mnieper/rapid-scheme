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

(define-record-type (<primitive-references> <expression>)
  (make-primitive-reference symbol syntax)
  primitive-reference?
  (symbol primitive-reference-symbol))

;;; Extra types

;; Variable bindings

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
    (%make-formals fixed #f rest))))
