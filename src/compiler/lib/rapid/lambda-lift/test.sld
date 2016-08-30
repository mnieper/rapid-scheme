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

(define-library (rapid lambda-lift test)
  (export run-tests)
  (import (rapid base)
          (rapid test)
	  (rapid expressions)
	  (rapid syntactic-environments)
	  (rapid lambda-lift))
  (begin
    (define (run-tests)
      (test-begin "Lambda lift")

      (test-equal "Lambda lift"
		  #(((letrec ((g0 g0))
		       g0)
		     g1)
		    (letrec ((g0 g0))
		      (g0 g1)))
		  (let*
		      ((g0 (make-location 0 #f))
		       (g1 (make-location 1 #f))
		       (exp
			(make-procedure-call
			 (make-letrec-expression
			  (list
			   (make-variables (make-formals (list g0) (list) #f)
					   (make-reference g0 #f)
					   #f))
			  (list (make-reference g0 #f))
			  #f)
			 (list
			  (make-reference g1 #f))
			 #f)))
		    (vector
		     (expression->datum exp)
		     (expression->datum (lambda-lift exp)))))

      (test-end))))
