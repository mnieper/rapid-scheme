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

(define-library (rapid compiler free-variables test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid sort)
	  (rapid iset)
	  (rapid receive)	 
	  (rapid compiler free-variables))
  (begin
    (define (run-tests)
      
      (test-begin "Free variables")

      (test-equal "free-variables"
	'(a z)
	(let ((expr '(if x
			 (receive (y) (+ x a)
			   (receive (z) (- 1 2)
			     (letrec ((a (lambda (x)
					   (f a x z))))			      
			       (f x y z))))
			 y)))
	  (receive (env)
	      (make-free-variables-environment expr)
	    (normalize-iset (free-variables 'a env)))))
      
      (test-end))

    (define (normalize-iset set)
      (sort (iset->list set)
	    (lambda (x y)
	      (string<? (symbol->string x)
			(symbol->string y)))))))
