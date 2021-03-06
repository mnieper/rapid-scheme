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

(define (vector-fold kons knil vector)
  (define n (vector-length vector))
  (let loop ((state knil) (i 0))
    (if (< i n)
	(loop (kons state (vector-ref vector i))
	      (+ i 1))
	state)))

(define (vector-index pred? vector)
  (define n (vector-length vector))
  (let loop ((i 0))
    (and (< i n)
	 (if (pred? (vector-ref vector i))
	     i
	     (loop (+ i 1))))))

(define (vector-every pred? vector)
  (let ((length (vector-length vector)))
    (let loop ((i 0))
      (cond
       ((= i length)
	#t)
       ((pred? (vector-ref vector i))
	(loop (+ 1 i)))
       (else
	#f)))))

(define (vector-any pred? vector)
  (let ((length (vector-length vector)))
    (let loop ((i 0))
      (cond
       ((= i length)
	#f)
       ((pred? (vector-ref vector i))
	#t)
       (else
	(loop (+ 1 i)))))))
