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

;;; Denotations

(define-record-type <denotation>
  #f
  denotation?
  (value denotation-value)
  (syntax denotation-syntax))

(define-record-type (<primitive> <denotation>)
  (make-primitive value syntax)
  primitive?)

;; TODO: Add denotation for transformers
;; TODO: Add denotation for locations

;;; Syntactic bindings

(define-record-type <syntactic-binding>
  (%make-syntactic-binding syntax denotation scope reference-count)
  syntactic-binding?
  (syntax binding-syntax)
  (denotation binding-denotation)
  (scope binding-scope)
  (reference-count binding-reference-count binding-set-reference-count!))

(define (make-syntactic-binding syntax denotation)
  (%make-syntactic-binding syntax denotation current-syntactic-environment 0))

;;; Syntactic environments

(define-record-type <syntactic-environment>
  (%make-syntactic-environment bindings)
  syntactic-environment?
  (bindings syntactic-environment-bindings
	    syntactic-environment-set-bindings!))

(define current-syntactic-environment (make-parameter #f))

(define (get-current-bindings)
  (syntactic-environment-bindings (current-syntactic-environment)))
(define (set-current-bindings! bindings)
  (syntactic-environment-set-bindings! (current-syntactic-environment)))

(define-syntax with-syntactic-environment
  (syntax-rules ()
    ((with-syntactic-environment syntactic-environment
       body1 body2 ...)
     (parameterize ((current-syntactic-environment
		     syntactic-environment))
       body1 body2 ...))))
