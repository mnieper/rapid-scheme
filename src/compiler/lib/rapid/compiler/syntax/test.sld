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

(define-library (rapid compiler syntax test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid compiler syntax))
  (begin
    (define (run-tests)
      (test-begin "rapid compiler syntax")

      (test-assert "make-syntax"
	(syntax? (make-syntax #f #f)))

      (test-equal "unwrap-syntax"
	'foo
	(unwrap-syntax (make-syntax 'foo #f)))

      (test-equal "syntax-context"
	'context
	(syntax-context (make-syntax #f 'context)))

      (test-equal "derive-syntax"
	(list 'foo 'bar)
	(let ((syntax
	       (derive-syntax (make-syntax 'foo 'quux) 'bar)))
	  (list (unwrap-syntax syntax)
		(syntax-context syntax))))

      (test-assert "derive-syntax: lists"
	(syntax?
	 (derive-syntax '(a b c) #f)))
      
      (test-equal "syntax->datum"
	'(1 2)
	(syntax->datum (derive-syntax '(1 2) #f)))
      
      #;
      (test-equal
	  '(1 2 3)
	(syntax->datum
	 (syntax-match
	  `(derive-syntax '(1 2 3) #f)
	  (,x x))))

      (test-end))))


