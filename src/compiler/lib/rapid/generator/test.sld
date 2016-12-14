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

(define-library (rapid generator test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid generator))
  (begin
    (define (run-tests)

      (test-begin "Generators")

      (test-begin "make-coroutine-generator")

      (test-equal "Coroutine generators"
		  '(2 1 0)
		  (let ((g (make-coroutine-generator
			    (lambda (yield)
			      (let loop ((i 0))
				(when (< i 3) (yield i)
				  (loop (+ i 1))))))))
		    (generator-fold cons '() g)))
      
      (test-end "make-coroutine-generator")

      (test-begin "make-range-generator")
      
      (test-equal "Range generators"
		  '(7 5 3)
		  (generator-fold cons '() (make-range-generator 3 8 2)))

      (test-end "make-range-generator")

      (test-assert "generator"
		   (eof-object? ((generator))))

      (test-equal "generator-for-each"
		  '(3 2 1)
		  (let ((list '())
			(generator (make-range-generator 1 4)))
		    (generator-for-each (lambda (element)
					  (set! list (cons element list)))
					generator)
		    list))
      
      (test-begin "gappend")
      
      (test-equal "Append generators"
		  '(1 0 2 1 0)
		  (generator-fold cons '() (gappend (make-range-generator 0 3)
						    (make-range-generator 0 2))))
      
      (test-end "gappend")
      
      (test-end "Generators"))))
