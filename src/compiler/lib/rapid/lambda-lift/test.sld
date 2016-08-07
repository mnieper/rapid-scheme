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
	  (rapid and-let)
	  (rapid receive)
	  (rapid lists)
	  (rapid test)
	  (rapid expressions)
	  (rapid library-definitions)
	  (rapid expand-library)
	  (rapid lambda-lift))
  (begin
    (define (run-tests)
      (test-begin "Lambda Lifting")

      ;; FIXME: The following test has to be rewritten as the
      ;; subsequent passes to lambda lifting mix up the result of
      ;; lambda lifting.
      #;(test-assert "Procedures without closure variables are lifted"
		   (parameterize
		       ((current-library-directories
			 (cons "./share" (current-library-directories))))
		     (receive (_ expression)
			 (expand-library (read-program "data/lambda-lift.scm"))
		       (any
			(lambda (variables)
			  (and-let*
			      ((expression (variables-expression variables))
			       ((expression-procedure? expression))
			       (clauses (procedure-clauses expression))
			       ((not (null? clauses)))
			       (expression
				(car
				 (letrec*-expression-body
				  (car (clause-body (car clauses))))))
			       ((literal? expression)))
			    (equal? 'foo (literal-datum expression))))
			(letrec*-expression-definitions expression)))))
      
      (test-end))))
