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

(define-library (rapid read test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid syntax)
	  (rapid read))
  (begin
    (define (source-port-string string)
      (make-source-port (open-input-string string) "stdin" #f))
    
    (define (read-datum string)
      (define port (source-port-string string))
      (syntax->datum
       (with-syntax-exception-handler
	(lambda ()
	  (read-syntax port #f)))))

    (define (run-tests)
      (test-begin "Reader")

      (test-assert "Source ports"
		   (source-port? (source-port-string "")))

      (test-equal "Read simple string"
		  "scheme"
		  (read-datum "\"scheme\""))

      (test-equal "White space"
		  "scheme"
		  (read-datum " \"scheme\" "))

      (test-equal "Line comments"
		  "scheme"
		  (read-datum ";comment\n\"scheme\""))

      (test-equal "Nested comments"
		  "scheme"
		  (read-datum "#|#|comment|#|#\"scheme\""))

      (test-equal "Datum comments"
		  "scheme"
		  (read-datum "#;\"comment\"\"scheme\""))

      (test-equal "Escapes in strings"
		  "\"scheme\""
		  (read-datum "\"\\\"scheme\\\"\""))

      (test-equal "Hex escapes in strings"
		  "A"
		  (read-datum "\"\\x41;\""))

      (test-equal "Line endings in strings"
		  "x\nscheme\nx"
		  (read-datum "\"x\nscheme\r\nx\""))

      (test-equal "Line endings preceded by backslash"
		  "Here’s text containing just one line"
		  (read-datum "\"Here’s text \\\n  containing just one line\""))

      (test-equal "Identifiers enclosed in vertical lines"
		  '|An identifier|
		  (read-datum "|\\x41;n identifier|"))
      
      (test-end "Reader"))))
