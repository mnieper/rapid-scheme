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

(define-library (rapid expand-library test)
  (export run-tests)
  (import (rapid base) (scheme write)
	  (rapid test)
	  (rapid syntax)
	  (rapid libraries)
	  (rapid expand-library))
  (begin
    (define (run-tests)
      (test-begin "Environments")

		   (parameterize
		       ((current-library-directories
			 (cons "./share" (current-library-directories))))
		     (expand-library (read-program "tests.scm"))
		     (zero? (error-message-count)))

      
      (test-assert "expand-library"
		   (parameterize
		       ((current-library-directories
			 (cons "./share" (current-library-directories))))
		     (with-syntax-exception-handler
		      (lambda ()
			(expand-library (read-program "tests.scm"))
			(zero? (error-message-count))))))

      (test-end))))
