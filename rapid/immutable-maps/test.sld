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

(define-library (rapid immutable-maps test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid comparators)
	  (rapid immutable-maps))
  (begin
    (define (run-tests)
      (test-begin "Immutable maps")

      (test-assert "Constructing an immutable maps yields an immutable map"
		   (imap? (imap (make-eq-comparator))))

      (test-equal "Replacing associations in a map"
		  2
		  (imap-ref (imap-replace (imap (make-eq-comparator) 'a 1)
					  'a
					  2)
			    'a))

      (test-error "Trying to reference non-associated keys raises an error"
		  (imap-ref (imap (make-eq-comparator) 'a 1) 'b))
      
      (test-end)
      #t)))
