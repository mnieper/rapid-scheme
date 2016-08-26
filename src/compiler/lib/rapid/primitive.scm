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

;; Mutable cells

(define (make-cell obj)
  (vector obj))

(define (cell-ref cell)
  (vector-ref cell 0))

(define (cell-set! cell value)
  (vector-set! cell 0 value))

;; Procedural records

(define-record-type <rtd>
  (%make-rtd size make-record record? record-fields)
  rtd?
  (size rtd-size)
  (make-record rtd-make-record)
  (record? rtd-record?)
  (record-fields rtd-record-fields))

(define (make-rtd size)
  (define-record-type <record>
    (make-record fields)
    record?
    (fields record-fields))
  (%make-rtd size make-record record? record-fields))

(define (make-constructor rtd indices)
  (let*
      ((make-record
        (rtd-make-record rtd))
       (size
        (rtd-size rtd)))
    (lambda (cont flag marks . args)
      (let ((arg-vector (list->vector args)))
        (unless (= (vector-length arg-vector) (vector-length indices))
          (error "unexpected number of arguments in record constructor"
		 (vector-length args)))
        (let ((fields (make-vector size (if #f #f))))
          (vector-for-each
           (lambda (arg index)
             (vector-set! fields index arg))
           arg-vector indices)
          (cont (make-record fields)))))))

 (define (make-predicate rtd)
  (let ((pred (rtd-record? rtd)))
    (lambda (cont flag marks obj)
      (cont (pred obj)))))

(define (make-accessor rtd index) 
  (let ((record-fields (rtd-record-fields rtd)))
    (lambda (cont flag marks record)
      (cont (vector-ref (record-fields record) index)))))

(define (make-mutator rtd index)
  (let*
      ((record-fields (rtd-record-fields rtd)))
    (lambda (cont flag marks record value)
      (cont (vector-set! (record-fields record) index value)))))

;;; Closure-related primitives

(define (make-closure . elements)
  (apply vector elements))

(define (closure-ref closure index)
  (vector-ref closure index))

(define (closure->code-pointer closure index)
  (cons (closure-ref closure index) closure))

(define (call code-pointer . operands)
  (apply (car code-pointer) (cdr code-pointer) operands))

