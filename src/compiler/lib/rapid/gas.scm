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

(define (write-label label)
  (write-string label)
  (write-char #\:)
  (newline))

(define (write-directive directive . operands)
  (write-char #\tab)
  (write-char #\.)
  (write-string directive)
  (write-char #\tab)
  (do ((operands operands (cdr operands))
       (sep "" ", "))
      ((null? operands))
    (write-string sep)
    (write-string (car operands)))
  (newline))

(define (write-instruction instruction . operands)
  (write-char #\tab)
  (write-string instruction)
  (write-char #\tab)
  (do ((operands operands (cdr operands))
       (sep "" ", "))
      ((null? operands))
    (write-string sep)
    (write-string (car operands)))
  (newline))

(define (number->hex number)
  (string-append "0x" (number->string number 16)))
