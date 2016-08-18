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

(define (bind-procedures exp)
  (parameterize
      ((current-letrec-expression-method
	(lambda (exp)
	  (make-letrec-expression
	   (map (lambda (variables)
		  (make-variables
		   (variables-formals variables)
		   (transform-procedure (variables-expression variables))
		   (variables-syntax variables)))
		(letrec-expression-definitions exp))
	   (letrec-expression-body exp)
	   #f)))
       (current-procedure-method
	(lambda (exp)
	  (let ((f (make-location #f)))
	    (make-letrec-expression
	     (list
	      (make-variables
	       (make-formals (list f) '() #f)
	       (transform-procedure exp)
	       #f))
	     (list
	      (make-reference f #f))
	     #f)))))
    (transform exp)))

(define (transform exp)
  (expression-dispatch exp))

(define (transform* exp)
  (expression-dispatch* exp))

(define (transform-procedure procedure)
  (make-procedure
   (map (lambda (clause)
	  (make-clause
	   (clause-formals clause)
	   (transform* (clause-body clause))
	   (clause-syntax clause)))
	(procedure-clauses procedure))
   procedure))
