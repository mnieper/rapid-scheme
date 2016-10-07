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

(define (bytevector-integer-set! bytevector k integer size)
  (let ((integer (if (negative? integer)
		     (+ (expt 256 size) integer)
		     integer)))
    (let loop ((size size)
	       (k k)
	       (integer integer))
      (unless (= size 0)
	(let ((byte (remainder integer 256)))
	  (bytevector-u8-set! bytevector k byte)
	  (loop (- size 1) (+ k 1) (quotient integer 256)))))))

(define (bytevector-integer-ref bytevector k size)
  (let loop ((integer 0) (k (+ k size -1)) (size size))
    (if (= size 0)
	integer
	(loop (+ (* integer 256) (bytevector-u8-ref bytevector k))
	      (- k 1)
	      (- size 1)))))

(define (integer->bytevector integer size)
  (let ((bytevector (make-bytevector size)))
    (bytevector-integer-set! bytevector 0 integer size)
    bytevector))

(define (bytevector->integer bytevector)
  (bytevector-integer-ref bytevector 0 (bytevector-length bytevector)))
