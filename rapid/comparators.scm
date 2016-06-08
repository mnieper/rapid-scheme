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

(define-record-type <comparator>
  (%make-comparator type-test equality ordering hash)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function))

(define (make-comparator type-test equality ordering hash)
  (%make-comparator
   type-test
   equality
   (if ordering ordering (lambda (x y) (error "ordering not supported")))
   (if hash hash (lambda (x) (error "hashing not supported")))))

(define (make-eq-comparator)
  (make-comparator (lambda (obj) #t) eq? #f #f))

(define (make-eqv-comparator)
  (make-comparator (lambda (obj) #t) eqv? #f #f))

(define (make-equal-comparator)
  (make-comparator (lambda (obj) #t) equal? #f #f))

(define (comparator-test-type comparator obj)
  ((comparator-type-test-predicate comparator) obj))

(define (comparator-check-type comparator obj)
  (or (comparator-test-type comparator obj)
      (error "comparator-check-type: invalid object" obj)))

(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))

(define (=? comparator obj1 obj2 . obj*)
  (define equality (comparator-equality-predicate comparator))
  (let loop ((obj1 obj1) (obj2 obj2) (obj* obj*))
    (and (or (null? obj*) (loop obj2 (car obj*) (cdr obj*)))
	 (equality obj1 obj2))))

(define (<? comparator obj1 obj2 . obj*)
  (let ((ordering (comparator-ordering-predicate comparator)))
    (let loop ((obj1 obj1) (obj2 obj2) (obj* obj*))
      (and (or (null? obj*) (loop obj2 (car obj*) (cdr obj*)))
	   (ordering obj1 obj2)))))

(define-syntax comparator-if<=>
  (syntax-rules ()
    ((comparator-if<=> object1 object2 less-than equal-to greater-than)
     (comparator-if<=> (make-default-comparator)
	 object1 object2 less-than equal-to greater-than))
    ((comparator-if<=> comparator object1 object2
       less-than
       equal-to
       greater-than)
     (let ((c comparator) (o1 object1) (o2 object2))
       (cond
	((<? c o1 o2)
	 less-than)
	((=? c o1 o2)
	 equal-to)
	(else
	 greater-than))))))
      
;; Local Variables:
;; eval: (put 'comparator-if<=> 'scheme-indent-function 3)
;; End:
