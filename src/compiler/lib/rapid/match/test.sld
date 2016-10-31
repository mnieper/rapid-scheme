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

      (test-begin "Guard expressions")

      (test-assert "Static guard"
	(match 'x
	  (x (guard #f) #f)
	  (x (guard #t) #t)))      

      (test-end)
	
      
      (test-end))))

	  