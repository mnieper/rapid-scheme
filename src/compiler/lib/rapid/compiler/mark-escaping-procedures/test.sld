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

(define-library (rapid compiler mark-escaping-procedures test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid compiler procedure-store)
	  (rapid compiler mark-escaping-procedures))
  (begin
    (define (run-tests)
      (test-begin "Mark escaping procedures")

      (test-equal "mark-escaping-procedures!"
	'(#f #t #f #t)
	(let ((expr '(letrec ((f (lambda (x) x))
			      (g (lambda (y) y))
			      (h (lambda (z) z)))
		       (+ (f g a)
			  (g h a)))))
	  (let ((store (make-procedure-store)))
	    (mark-escaping-procedures! expr store)
	    (list (escaping-procedure? 'f store)
		  (escaping-procedure? 'g store)
		  (continuation? 'g store)
		  (continuation? 'h store)))))
      
      (test-end))))
