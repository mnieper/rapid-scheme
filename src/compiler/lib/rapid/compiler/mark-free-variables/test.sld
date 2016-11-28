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

(define-library (rapid compiler mark-free-variables test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid receive)
	  (rapid compiler mark-free-variables))
  (begin
    (define (run-tests)
      (test-begin "Mark free variables")

      (test-equal "mark-free-variables"
	'((if x (let (free x) ((y (+ x a)))
	 	   (let (free y x) ((z (- 1 2)))
		      (f x y z)))
              y)
	  (y x))
	(receive result	
	    (mark-free-variables
	     '(if x (let ((y (+ x a)))
		      (let ((z (- 1 2)))
			(f x y z)))
		  y)
	     '(x y z))
	  result))

      (test-end))))
