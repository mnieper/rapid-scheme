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

(define-record-type <iset>
  (%iset comparator elements)
  iset?
  (comparator iset-comparator)
  (elements iset-elements))

;; TODO: Use an efficient functional data structure.

(define (iset comparator . object*)
  (let loop
      ((set
	(%iset comparator '()))
       (object* object*))
    (if (null? object*)
	set
	(loop (iset-adjoin set (car object*)) (cdr object*)))))

(define (iset-member? set obj)
  (and (member obj
	       (iset-elements set)
	       (comparator-equality-predicate (iset-comparator set)))
      #t))

(define (iset-adjoin set obj)
  (%iset (iset-comparator set) (cons obj (iset-elements set))))
