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

(define-syntactic-environment primitive-environment

  (define-transformer (quote syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 (or (= (length form) 2)
	     (begin (raise-syntax-error syntax
					"bad quote syntax")
		    #f)))
      (expand-into-expression
       (make-literal (syntax->datum (list-ref form 1)) syntax))))
  
  (define-transformer (define-primitive syntax)
    (and-let*
	((form (unwrap-syntax syntax))
	 ((or (= (length form) 3)
	      (begin (raise-syntax-error syntax
					 "bad define-primitive syntax")
		     #f)))
	 (identifier-syntax (list-ref form 1))
	 (identifier (unwrap-syntax identifier-syntax))
	 ((or (identifier? identifier)
	      (begin (raise-syntax-error identifier-syntax
					 "identifier expected")
		     #f)))
	 (literal-syntax (list-ref form 2))
	 (literal (expand-expression literal-syntax))
	 ((or (literal? literal)
	      (begin (raise-syntax-error literal-syntax
					 "literal symbol expected")
		     #f)))
	 (symbol (literal-datum literal))
	 ((or (symbol? symbol)
	      (begin (raise-syntax-error literal-syntax
					 "symbol expected")
		     #f))))
      (expand-into-syntax-definition identifier-syntax
				     (make-primitive-reference symbol
							       literal-syntax)
				     syntax))))
