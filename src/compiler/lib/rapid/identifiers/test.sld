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

(define-library (rapid identifiers test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid identifiers))
  (begin
    (define (run-tests)
      (test-begin "Identifiers")

      (test-assert "identifier?"
		   (identifier? (make-synthetic-identifier 'x)))

      (test-equal "identifier->symbol"
		  'x
		  (identifier->symbol (make-synthetic-identifier 'x)))
      
      (test-equal "symbol->identifier"
		  'x
		  (identifier->symbol (symbol->identifier 'x)))

      (test-equal "bound-identifier=?"
		  '(#t #f #f #t #f)
		  (let ((id1 (symbol->identifier 'x))
			(id2 (symbol->identifier 'x))
			(id3 (symbol->identifier 'y))
			(id4 (make-synthetic-identifier 'x))
			(id5 (make-synthetic-identifier 'x)))
		    (list
		     (bound-identifier=? id1 id2)
		     (bound-identifier=? id1 id3)
		     (bound-identifier=? id1 id4)
		     (bound-identifier=? id4 id4)
		     (bound-identifier=? id4 id5))))

      (test-end "Identifiers"))))
