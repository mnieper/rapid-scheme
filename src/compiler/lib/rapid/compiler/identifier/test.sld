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

(define-library (rapid compiler identifier test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid compiler identifier))
  (begin
    (define (run-tests)
      (test-begin "Identifiers")

      (test-assert "make-synthetic-identifier"
	(synthetic-identifier? (make-synthetic-identifier 'foo)))

      (test-assert "Symbols are identifiers"
	(identifier? 'bar))

      (test-assert "Synthetic identifiers are identifiers"
	(identifier? (make-synthetic-identifier 'baz)))

      (test-assert "The same identifier is always mapped to the same symbol"
	(let ((identifier (make-synthetic-identifier 'quux)))
	  (eq? (identifier->symbol identifier)
	       (identifier->symbol identifier))))

      (test-assert "Different synthetic identifiers are mapped to different symbols"
	(not (eq? (make-synthetic-identifier 'waldo)
		  (make-synthetic-identifier 'waldo))))

      (test-equal "make-renaming-procedure"
	'(#t #f)
	(let ((rename (make-renaming-procedure)))
	  (let ((identifier1 (rename 'wibble))
		(identifier2 (rename 'wobble)))
	    (list (eq? identifier1 (rename 'wibble)) (eq? identifier1 identifier2)))))		

      (test-end))))
