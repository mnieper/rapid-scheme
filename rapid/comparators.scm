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

;;; Syntax

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

(define (string-hash obj)
  (let ((acc (make-hasher))
        (len (string-length obj)))
    (do ((n 0 (+ n 1)))
	((= n len) (acc))
      (acc (char->integer (string-ref obj n))))))

(define (symbol-hash symbol)
  (string-hash (symbol->string symbol)))

(define (number-hash obj)
  (cond
   ((nan? obj)
    (salt))
   ((infinite? obj)
    (if (positive? obj)
	(* 2 (salt))
	(* 3 (salt))))
   ((real? obj)
    (abs (exact (round obj))))
   (else
    (+ (number-hash (real-part obj))
       (number-hash (imag-part obj))))))

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

;;; List comparators

(define (make-list-comparator element-comparator type-test empty? head tail)
  (make-comparator
   (make-list-type-test element-comparator type-test empty? head tail)
   (make-list=? element-comparator type-test empty? head tail)
   (make-list<? element-comparator type-test empty? head tail)
   (make-list-hash element-comparator type-test empty? head tail)))

(define (make-list-type-test element-comparator type-test empty? head tail)
  (lambda (obj)
    (and
      (type-test obj)
      (let ((type-test (comparator-type-test-predicate element-comparator)))
        (let loop ((obj obj))
	  (or (empty? obj)
	      (and (type-test (head obj))
		   (loop (tail obj)))))))))

(define (make-list=? element-comparator type-test empty? head tail)
  (lambda (a b)
    (let ((=? (comparator-equality-predicate element-comparator)))
      (let loop ((a a) (b b))
	(or (and (empty? a) (empty? b))
	    (and (not (empty? a))
		 (not (empty? b))
		 (and (=? (head a) (head b))
		      (loop (tail a) (tail b)))))))))

(define (make-list<? element-comparator type-test empty? head tail)
  (lambda (a b)
    (let ((=? (comparator-equality-predicate element-comparator))
          (<? (comparator-ordering-predicate element-comparator)))
      (let loop ((a a) (b b))
	(and (not (and (empty? a) (empty? b)))
	     (not (empty? b))
	     (or (empty? a)
		 (if (=? (head a) (head b))
		     (loop (tail a) (tail b))
		     (<? (head a) (head b)))))))))

(define (make-list-hash element-comparator type-test empty? head tail)
  (lambda (obj)
    (let ((hash (comparator-hash-function element-comparator))
          (acc (make-hasher)))
      (let loop ((obj obj))
        (cond
	 ((empty? obj)
	  (acc))
	 (else
	  (acc (hash (head obj)))
	  (loop (tail obj))))))))

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

(define-syntax hash-bound
  (syntax-rules ()
    ((hash-bound) 33554432)))

(define salt (make-parameter 16064047))

(define-syntax hash-salt
  (syntax-rules ()
    ((hash-salt) (salt))))

(define (make-hasher)
  (let ((result (salt)))
    (case-lambda
     (() result)
     ((n) (set! result (+ (modulo (* result 33) (hash-bound)) n))
          result))))

;; Local Variables:
;; eval: (put 'comparator-if<=> 'scheme-indent-function 3)
;; End:
