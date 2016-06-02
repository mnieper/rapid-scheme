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

(define-library (rapid immutable-sets test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid comparators)
	  (rapid immutable-sets))
  (begin
    (define (run-tests)
      (test-begin "Immutable sets")

      (test-assert "Constructing an immutable set yields an immutable set"
		   (iset? (iset (make-eq-comparator))))

      (test-assert "An adjoined element is a member of a set"
		   (iset-member? (iset-adjoin (iset (make-eq-comparator)) 'a) 'a))

      (test-assert "A non-element is not a member"
		   (not (iset-member? (iset-adjoin (iset (make-eq-comparator)) 'a)
				      'b)))

      (test-assert "Constructor for non-empty sets"
		   (iset-member? (iset (make-eq-comparator) 'a 'b 'c) 'b))

      (test-end)
      #t)))
   
