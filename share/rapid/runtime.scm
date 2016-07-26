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

(define rtd-type (make-type 'rtd))

(define (make-rtd size)
  (let ((instance-type (make-type 'record)))
    (make-instance rtd-type (vector instance-type size))))

(define (make-constructor rtd indices)
  (let*
      ((rtd-payload (instance-ref rtd))
       (instance-type (vector-ref rtd-payload 0))
       (size (vector-ref rtd-payload 1)))
    (lambda args
      (let ((arg-vector (list->vector args)))
	(unless (= (vector-length arg-vector) (vector-length indices))
	  (error "unexpected number of arguments in record constructor"
		 (vector-length args)))
	(let ((fields (make-vector size (if #f #f))))
	  (vector-for-each
	   (lambda (arg index)
	     (vector-set! fields index arg))
	   arg-vector indices)
	  (make-instance instance-type fields))))))

(define (make-predicate rtd)
  (let ((instance-type (vector-ref (instance-ref rtd) 0)))
    (lambda (obj)
      (instance? instance-type obj))))

(define (make-accessor rtd index)
  (lambda (record)
    (vector-ref (instance-ref record) index)))

(define (make-mutator rtd index)
  (lambda (record value)
    (vector-set! (instance-ref record) index value)))
