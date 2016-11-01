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

(define-library (rapid match test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid match))
  (begin
    (define (run-tests)
      (test-begin "rapid match")

      (test-begin "Match literals")

      (test-assert "Match number"
	(match 2
	  (1 #f)
	  (2 #t)))

      (test-assert "Match symbol"
	(match 'x
	  (x #t)
	  (y #f)))

      (test-end)

      (test-begin "List matches")
      
      (test-assert "Match list"
	(match '(1 2)
	  ((,x ,y ,z) #f)
	  ((,x ,y) (eq? x 1))))

      (test-assert "Match empty list"
	(match '()
	  ((,x) #f)
	  (() #t)))

      ;; FIXME: Dotted tail and vars
      #;(test-assert "Match dotted list"
	(match '(1 2 3)
	  ((1 . 3) #f)
	  ((1 . ,w) (equal? w '(2 3)))))

      (test-end)

      (test-begin "Catamorphisms")

      (test-equal "Simple recursion"
	'foo
	(match '(begin (bar foo))
	  ((bar ,x) x)
	  ((begin ,(x)) x)))

      ; FIXME: see above
      #;(test-equal "Splitting values"
	'((a c e) (b d f))
	(let-values
	    ((result 
	      (match '(a b c d e f)
		(() (values '() '()))
		((,x) (values `(,x) '()))
		((,x ,y . ,(odds evens))
		 (values (cons x odds)
			 (cons y evens))))))
	  result))
      
      (test-end)
      
      (test-begin "Guard expressions")

      (test-assert "Static guard"
	(match 'x
	  (x (guard #f) #f)
	  (x (guard #t) #t)))      

      (test-end)

      (test-begin "Pattern variables")

      (test-equal "Simple pattern variable"
	'foo
	(match 'foo	 
	  (x #f)
	  (,x x)))
      
      (test-end)

      (test-end "rapid match"))))

	  
