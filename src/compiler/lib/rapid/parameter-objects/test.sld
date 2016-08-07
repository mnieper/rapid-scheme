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

(define-library (rapid parameter-objects test)
  (import (rapid base)
	  (rapid test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "Parameter objects")

      (test-equal "Parameters can be parameterized, set and are restored"
		  '(10 20 30 10)
		  (let ((p (make-parameter 1 (lambda (x) (* x 10)))))
		    (cons (p)
			  (append (parameterize ((p 2))
				    (let ((y (p)))
				      (list y
					    (begin (p 3)
						   (p)))))
				  (list (p))))))

      (test-end)
      #t)))
