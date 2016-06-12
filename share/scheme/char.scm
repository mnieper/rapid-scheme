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

(define-primitive char-alphabetic? 'char-alphabetic)
(define-primitive char-ci<=? 'char-ci<=?)
(define-primitive char-ci<? 'char-ci<?)
(define-primitive char-ci=? 'char-ci=?)
(define-primitive char-ci>=? 'char-ci>=?)
(define-primitive char-ci>? 'char-ci>?)
(define-primitive char-downcase 'char-downcase)
(define-primitive char-foldcase 'char-foldcase)
(define-primitive char-lower-case? 'char-lower-case?)
(define-primitive char-numeric? 'char-numeric?)
(define-primitive char-upcase 'char-upcase)
(define-primitive char-upper-case? 'char-upper-case?)
(define-primitive char-whitespace? 'char-whitespace?)
(define-primitive digit-value 'digit-value)
(define-primitive string-ci<=? 'string-ci<=?)
(define-primitive string-ci<? 'string-ci<?)
(define-primitive string-ci=? 'string-ci=?)
(define-primitive string-ci>=? 'string-ci>=?)
(define-primitive string-ci>? 'string-ci>?)
(define-primitive string-downcase 'string-downcase)
(define-primitive string-foldcase 'string-foldcase)
(define-primitive string-upcase 'string-upcase)
