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

(define-library (rapid compiler partition-bindings test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid compiler free-variables)
	  (rapid compiler partition-bindings))
  (begin
    (define (run-tests)
      (test-begin "Partition Bindings")

      (test-equal "partition-bindings: Basic example"
	'x
	(let ((expr 'x))
	  (let ((env (make-free-variables-environment expr)))
	    (partition-bindings expr env))))

      (test-equal "partition-bindings: Keep, Hearn, Dybvig example"
	'(letrec ((main
		   (lambda (x)
		     (letrec ((f (lambda (a) (a x))))
		       (letrec ((g (lambda () (f (h x))))
				(h (lambda (z) (g))))
			 (letrec ((q (lambda (y) (+ (length y) 1))))
			   (q (g))))))))
	   main)
	(let ((expr '(letrec ((main
			       (lambda (x)
				 (letrec ((f (lambda (a) (a x)))
					  (g (lambda () (f (h x))))
					  (h (lambda (z) (g)))
					  (q (lambda (y) (+ (length y) 1))))
				   (q (g))))))
		       main)))
	  (let ((env (make-free-variables-environment expr)))
	    (partition-bindings expr env))))
      
      (test-end))))
