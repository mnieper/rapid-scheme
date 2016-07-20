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

(define-library (rapid expressions test)
  (export run-tests)
  (import (scheme base)
	  (scheme case-lambda)
	  (rapid test)
	  (rapid syntactic-environments)
	  (rapid expressions))
  (begin
    (define (run-tests)
      (test-begin "Expressions")

      (test-equal "Quote expressions"
		  '(if #t
		       (set! g0 (list 'foo '(1 . 2)))
		       (if #f #f))
		  (let ((g0 (make-location 0 #f))
			(list (make-primitive 'list #f)))
		    (expression->datum
		     (expression
		      (if '#t
			  (set! g0 (list ,(make-literal 'foo #f) '(1 . 2))))))))

      (test-equal "Quoting procedures"
		  '(case-lambda
		    ((g0) 42))
		  (expression->datum
		   (expression
		    (case-lambda
		     ,@(list (make-clause (make-formals (list (make-location 0 #f)) #f)
					  (list (expression '42))
					  #f))))))
		   
      (test-end))))
