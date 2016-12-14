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

(define-library (rapid comparator)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme complex)
	  (scheme char)
	  (scheme inexact))
  (export comparator? comparator-ordered? comparator-hashable?
	  make-comparator
	  make-pair-comparator make-list-comparator make-vector-comparator
	  make-eq-comparator make-eqv-comparator make-equal-comparator
	  boolean-hash char-hash char-ci-hash
	  string-hash string-ci-hash symbol-hash number-hash
	  make-default-comparator default-hash comparator-register-default!
	  comparator-type-test-predicate comparator-equality-predicate
	  comparator-ordering-predicate comparator-hash-function
	  comparator-test-type comparator-check-type comparator-hash
	  hash-bound hash-salt
	  =? <? >? <=? >=?
	  comparator-if<=>)
  (include "comparator.scm"))

;; Local Variables:
;; eval: (put 'comparator-if<=> 'scheme-indent-function 3)
;; End:
