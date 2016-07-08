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

;; Fundamental binding construct

(define-syntax letrec*-values
  (syntax-rules ()
    ((letrec*-values ((formals init) ...) body1 body2 ...)
     (let ()
       (define-values formals init)
       ...
       (let ()
	 body1
	 body2
	 ...)))))

;; Set multiple values

(define-syntax set!-values
  (syntax-rules ()
    ((set!-values formals expression)
     (set!-values-aux formals () () expression))))

(define-syntax set!-values-aux
  (syntax-rules ()
    ((set!-values-aux () new-formals ((var new) ...) expression)
     (let-values ((new-formals expression))
       (set! var new)
       ...))
    ((set!-values-aux (var . formals) (new-formal ...) (tmp ...) expression)
     (set!-values-aux formals (new-formal ... new) (tmp ... (var new)) expression))
    ((set!-values-aux var new-formals (tmp ...) expression)
     (set!-values-aux () (new-formals . var) (tmp ... (var new)) expression))))

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
    (lambda args
      (let ((arg-vector (list->vector args)))
	(unless (= (vector-length arg-vector) (vector-length indices))
	  (error "unexpected number of arguments in record constructor" (vector-length args)))
	(let ((fields (make-vector size (if #f #f))))
	  (vector-for-each
	   (lambda (arg index)
	     (vector-set! fields index arg))
	   arg-vector indices)
	  (make-record fields))))))

(define (make-predicate rtd)
  (let ((pred (rtd-record? rtd)))
    (lambda (obj)
      (pred obj))))

(define (make-accessor rtd index) 
  (let ((record-fields (rtd-record-fields rtd)))
    (lambda (record)
      (vector-ref (record-fields record) index))))

(define (make-mutator rtd index)
  (let*
      ((record-fields (rtd-record-fields rtd)))
    (lambda (record value)
      (vector-set! (record-fields record) index value))))
