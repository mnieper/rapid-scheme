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

(import (rapid primitive))

(define-primitive display 'display)
(define-primitive newline 'newline)
(define-primitive zero? 'zero?)
(define-primitive * '*)
(define-primitive - '-)
(define-primitive wcm 'wcm)
(define-primitive ccm 'ccm)

(define-values (fac)
  (case-lambda
   ((n)
    (if (zero? n)
	(begin
	  (display (ccm))
	  (newline)
	  1)
	(wcm n (* n (fac (- n 1))))))))
(fac 2)

(define-values (fac/tail-recursive)
  (case-lambda
   ((n acc)
    (if (zero? n)
	(begin
	  (display (ccm))
	  (newline)
	  acc)
	(wcm n (fac/tail-recursive (- n 1) (* n acc)))))))
(fac/tail-recursive 2 1)

;; Local Variables:
;; mode: scheme
;; End:
