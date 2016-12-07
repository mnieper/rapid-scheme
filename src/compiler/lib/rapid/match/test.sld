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

      (test-equal "Match number"
	2
	(match 2
	  (1 #f)
	  (2 2)))

      (test-equal "Match symbol"
	#t
	(match 'x
	  (x #t)
	  (y #f)))

      (test-end)

      (test-begin "List matches")
      
      (test-equal "Match list"
	1
	(match '(1 2)
	  ((,x) #f)
	  ((,x ,y ,z) #f)
	  ((,x ,y) x)))

      (test-equal "Match empty list"
	'empty
	(match '()
	  ((,x) #f)
	  (() 'empty)))

      (test-equal "Match dotted list"
	'(2 3)
	(match '(1 2 3)
	  ((1 . 3) #f)
	  ((1 . ,w) w)))

      (test-end)

      (test-begin "Catamorphisms")

      (test-equal "Simple recursion"
	'foo
	(match '(begin (bar foo))
	  ((bar ,x) x)
	  ((begin ,(x)) x)))

      (test-equal "Splitting values"
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

      (test-equal "More catamorphisms"
	'(2 3 4)
	(let ((add1 (lambda (x)
		      (+ x 1))))
	  (match '(if 1 2 3)
	    ((if ,(add1 -> x)
		 ,(add1 -> y)
		 ,(add1 -> z))
	     (list x y z))
	    (,_ #f))))
           
      (test-end)
      
      (test-begin "Guard expressions")

      (test-equal "Static guard"
	'x
	(match 'x
	  (x (guard #f) #f)
	  (x (guard #t) 'x)))      

      (test-end)

      (test-begin "Pattern variables")

      (test-equal "Simple pattern variable"
	'foo
	(match 'foo	 
	  (x #f)
	  (,x x)))
      
      (test-end)

      (test-begin "Ellipsis")

      (test-equal "Simple ellipsis case"
	'(foo bar)
	(match '(qux foo bar baz)
	  ((qux ,x ... bla) #f)
	  ((qux ,x ... baz) x)))

      (test-equal "Two variables before ellipsis"
	'((foo bar) (1 2))
	(match '(qux (foo 1) (bar 2) baz)
	  ((qux (,x ,y) ... baz) `(,x ,y))))

      (test-equal "More than one ellipsis"
	'((foo 1) (bar 2))
	(match '(qux (foo 1) (bar 2) baz)
	  ((qux (,x ...) ... baz) x)))

      (test-equal "Empty list"
	'()
	(match '(foo)
	  ((foo (,x* ,y*) ...)
	   (map list x* y*))
	  (,_ #f)))
      
      (test-end)

      (test-end "rapid match"))))

	  
