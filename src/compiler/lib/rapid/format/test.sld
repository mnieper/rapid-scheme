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

(define-library (rapid format test)
  (import (scheme base)
          (scheme write)
          (rapid test)
          (rapid format))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "Format")

      (test-assert "Values can be inserted as if printed with display"
                   (let ((p (open-output-string)))
                     (display "42" p)
                   (string=? (format "~a" "42") (get-output-string p))))

      (test-assert "Values can be inserted as if printed with write"
                   (let ((p (open-output-string)))
                     (write "43" p)
                   (string=? (format "~s" "43") (get-output-string p))))

      (test-equal "Newline escape sequence"
                  "\n"
                  (format "~%"))

      (test-equal "Tilde escape sequence"
                  "~"             
                  (format "~~"))

      (test-end)

      #t)))
