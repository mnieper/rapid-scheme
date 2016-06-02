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

(define-library (rapid vectors test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid vectors))
  (begin
    (define (run-tests)
      (test-begin "Vectors")

      (test-equal "Fundamental vector iterator"
		  3
		  (vector-fold (lambda (counter n) 
				 (if (even? n) (+ counter 1) counter)) 
			       0 #(1 2 3 4 5 6)))

      (test-equal "Index of first element satisfying predicate"
		  1
		  (vector-index even? #(1 2 3)))

      (test-assert "Trying to find non existing element yields false"
		   (not (vector-index even? #(1 3 5))))
	 
      (test-end)
      #t)))
