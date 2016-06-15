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

(define current-context (make-parameter #f))
(define (top-level-context?) (eq? (current-context) 'top-level))
(define (definition-context?) (eq? (current-context) 'definition))
(define (expression-context?) (eq? (current-context) 'expression))

(define (make-definition formals expression-syntax syntax)
  (vector formals expression-syntax syntax))
(define (definition-formals definition)
  (vector-ref definition 0))
(define (definition-expression-syntax definition)
  (vector-ref definition 1))
(define (definition-syntax definition)
  (vector-ref definition 2))

(define current-definitions (make-parameter #f))

(define (expand-top-level! syntax*)
  (parameterize
      ((current-context 'top-level)
       (current-definitions (list-queue)))
    (for-each expand-definition! syntax*)
    (current-context 'expression)
    (list-queue-map!
     (lambda (definition)
       (make-variables (definition-formals definition)
		       (expand-expression (definition-expression-syntax definition))
		       (definition-syntax definition)))
     (current-definitions))))

;; TODO: expand-body

(define (expand-definition! syntax)
  (maybe-isolate (top-level-context?)
    (lambda ()
      #f)))

(define (expand-expression syntax)
  (error "expand-expression: not implemented yet"))
