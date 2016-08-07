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

(define-library (rapid graphs test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid comparators)
	  (rapid graphs))
  (begin
    (define symbol-comparator
      (make-comparator symbol? symbol=?
		       (lambda (symbol1 symbol2)
			 (string<? (symbol->string symbol1)
				   (symbol->string symbol2)))
		       #f))
      
    (define (run-tests)
      (test-begin "Graphs")

      (test-equal "Fixing Letrec (reloaded) 1"
		  '((t) (g) (s) (r) (f) (q))
		  (let ((graph
			 '((t g s)
			   (g s r)
			   (s r f)
			   (r f q)
			   (f q)
			   (q))))
		    (graph-scc graph symbol-comparator)))

      (test-equal "Fixing Letrec (reloaded) 2"
		  '((t) (f) (e o))
		  (let ((graph
			 '((t f)
			   (f e)
			   (e o)
			   (o e))))	      
		    (graph-scc graph symbol-comparator)))

      (test-equal "Fixing Letrec (reloaded) 3"
		  '((t) (f) (y x))
		  (let ((graph
			 '((t f y)
			   (f y x)
			   (y x)
			   (x y))))
		    (graph-scc graph symbol-comparator)))

      (test-end))))
