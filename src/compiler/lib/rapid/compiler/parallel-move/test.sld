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

(define-library (rapid compiler parallel-move test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid compiler parallel-move))
  (begin
    (define (run-tests)
      (test-begin "rapid compiler parallel-move")

      (test-equal "parallel-move*"
	'((move a b) (xchg a c))
	(parallel-move* '(a a c) '(b c a)))

      (test-equal "parallel-move*: long cycle"
	'((move v x) (move b v) (move b u) (move a b) (move d e) (xchg a c d))
	(parallel-move* '(a a c d d b b v r) '(b c d a e u v x r)))
      
      (test-end))))
