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


;; Copyright (C) John Cowan (2015). All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (rapid comparator test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid comparator))
  (begin
    (define (run-tests)
      
      (test-begin "Comparators")

      (test-equal '#(2 3 4) (vector-cdr '#(1 2 3 4)))
      (test-equal '#() (vector-cdr '#(1)))
      
      (test-group "Comparators/predicates"
		  (test-assert (comparator? real-comparator))
		  (test-assert (not (comparator? =)))
		  (test-assert (comparator-ordered? real-comparator))
		  (test-assert (comparator-hashable? real-comparator))
		  (test-assert (not (comparator-ordered? degenerate-comparator)))
		  (test-assert (not (comparator-hashable? degenerate-comparator)))
		  )
      
      (test-group "Comparators/constructors"
		  (define bool-pair (cons #t #f))
		  (define bool-pair-2 (cons #t #f))
		  (define reverse-bool-pair (cons #f #t))

		  (test-assert (=? boolean-comparator #t #t))
		  (test-assert (not (=? boolean-comparator #t #f)))
		  (test-assert (<? boolean-comparator #f #t))
		  (test-assert (not (<? boolean-comparator #t #t)))
		  (test-assert (not (<? boolean-comparator #t #f)))

		  (test-assert (comparator-test-type bool-pair-comparator '(#t . #f)))
		  (test-assert (not (comparator-test-type bool-pair-comparator 32)))
		  (test-assert (not (comparator-test-type bool-pair-comparator '(32 . #f))))
		  (test-assert (not (comparator-test-type bool-pair-comparator '(#t . 32))))
		  (test-assert (not (comparator-test-type bool-pair-comparator '(32 . 34))))
		  (test-assert (=? bool-pair-comparator '(#t . #t) '(#t . #t)))
		  (test-assert (not (=? bool-pair-comparator '(#t . #t) '(#f . #t))))
		  (test-assert (not (=? bool-pair-comparator '(#t . #t) '(#t . #f))))
		  (test-assert (<? bool-pair-comparator '(#f . #t) '(#t . #t)))
		  (test-assert (<? bool-pair-comparator '(#t . #f) '(#t . #t)))
		  (test-assert (not (<? bool-pair-comparator '(#t . #t) '(#t . #t))))
		  (test-assert (not (<? bool-pair-comparator '(#t . #t) '(#f . #t))))
		  (test-assert (not (<? bool-pair-comparator '(#f . #t) '(#f . #f))))

		  (test-assert (comparator-test-type num-vector-comparator '#(1 2 3)))
		  (test-assert (comparator-test-type num-vector-comparator '#()))
		  (test-assert (not (comparator-test-type num-vector-comparator 1)))
		  (test-assert (not (comparator-test-type num-vector-comparator '#(a 2 3))))
		  (test-assert (not (comparator-test-type num-vector-comparator '#(1 b 3))))
		  (test-assert (not (comparator-test-type num-vector-comparator '#(1 2 c))))
		  (test-assert (=? num-vector-comparator '#(1 2 3) '#(1 2 3)))
		  (test-assert (not (=? num-vector-comparator '#(1 2 3) '#(4 5 6))))
		  (test-assert (not (=? num-vector-comparator '#(1 2 3) '#(1 5 6))))
		  (test-assert (not (=? num-vector-comparator '#(1 2 3) '#(1 2 6))))
		  (test-assert (<? num-vector-comparator '#(1 2) '#(1 2 3)))
		  (test-assert (<? num-vector-comparator '#(1 2 3) '#(2 3 4)))
		  (test-assert (<? num-vector-comparator '#(1 2 3) '#(1 3 4)))
		  (test-assert (<? num-vector-comparator '#(1 2 3) '#(1 2 4)))
		  (test-assert (<? num-vector-comparator '#(3 4) '#(1 2 3)))
		  (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(1 2 3))))
		  (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(1 2))))
		  (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(0 2 3))))
		  (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(1 1 3))))

		  (test-assert (not (<? vector-qua-list-comparator '#(3 4) '#(1 2 3))))
		  (test-assert (<? list-qua-vector-comparator '(3 4) '(1 2 3)))

		  (test-assert (=? eq-comparator #t #t))
		  (test-assert (not (=? eq-comparator #f #t)))
		  (test-assert (=? eqv-comparator bool-pair bool-pair))
		  (test-assert (not (=? eqv-comparator bool-pair bool-pair-2)))
		  (test-assert (=? equal-comparator bool-pair bool-pair-2))
		  (test-assert (not (=? equal-comparator bool-pair reverse-bool-pair)))
		  )

      (test-group "Comparators/hash"
		  (test-assert (exact-integer? (boolean-hash #f)))
		  (test-assert (not (negative? (boolean-hash #t))))
		  (test-assert (exact-integer? (char-hash #\a)))
		  (test-assert (not (negative? (char-hash #\b))))
		  (test-assert (exact-integer? (char-ci-hash #\a)))
		  (test-assert (not (negative? (char-ci-hash #\b))))
		  (test-assert (= (char-ci-hash #\a) (char-ci-hash #\A)))
		  (test-assert (exact-integer? (string-hash "f")))
		  (test-assert (not (negative? (string-hash "g"))))
		  (test-assert (exact-integer? (string-ci-hash "f")))
		  (test-assert (not (negative? (string-ci-hash "g"))))
		  (test-assert (= (string-ci-hash "f") (string-ci-hash "F")))
		  (test-assert (exact-integer? (symbol-hash 'f)))
		  (test-assert (not (negative? (symbol-hash 't))))
		  (test-assert (exact-integer? (number-hash 3)))
		  (test-assert (not (negative? (number-hash 3))))
		  (test-assert (exact-integer? (number-hash -3)))
		  (test-assert (not (negative? (number-hash -3))))
		  (test-assert (exact-integer? (number-hash 3.0)))
		  (test-assert (not (negative? (number-hash 3.0))))
		  )

      (test-group "Comparators/default"
		  (test-assert (<? default-comparator '() '(a)))
		  (test-assert (not (=? default-comparator '() '(a))))
		  (test-assert (=? default-comparator #t #t))
		  (test-assert (not (=? default-comparator #t #f)))
		  (test-assert (<? default-comparator #f #t))
		  (test-assert (not (<? default-comparator #t #t)))
		  (test-assert (=? default-comparator #\a #\a))
		  (test-assert (<? default-comparator #\a #\b))

		  (test-assert (comparator-test-type default-comparator '()))
		  (test-assert (comparator-test-type default-comparator #t))
		  (test-assert (comparator-test-type default-comparator #\t))
		  (test-assert (comparator-test-type default-comparator '(a)))
		  (test-assert (comparator-test-type default-comparator 'a))
		  (test-assert (comparator-test-type default-comparator (make-bytevector 10)))
		  (test-assert (comparator-test-type default-comparator 10))
		  (test-assert (comparator-test-type default-comparator 10.0))
		  (test-assert (comparator-test-type default-comparator "10.0"))
		  (test-assert (comparator-test-type default-comparator '#(10)))

		  (test-assert (=? default-comparator '(#t . #t) '(#t . #t)))
		  (test-assert (not (=? default-comparator '(#t . #t) '(#f . #t))))
		  (test-assert (not (=? default-comparator '(#t . #t) '(#t . #f))))
		  (test-assert (<? default-comparator '(#f . #t) '(#t . #t)))
		  (test-assert (<? default-comparator '(#t . #f) '(#t . #t)))
		  (test-assert (not (<? default-comparator '(#t . #t) '(#t . #t))))
		  (test-assert (not (<? default-comparator '(#t . #t) '(#f . #t))))
		  (test-assert (not (<? default-comparator '#(#f #t) '#(#f #f))))

		  (test-assert (=? default-comparator '#(#t #t) '#(#t #t)))
		  (test-assert (not (=? default-comparator '#(#t #t) '#(#f #t))))
		  (test-assert (not (=? default-comparator '#(#t #t) '#(#t #f))))
		  (test-assert (<? default-comparator '#(#f #t) '#(#t #t)))
		  (test-assert (<? default-comparator '#(#t #f) '#(#t #t)))
		  (test-assert (not (<? default-comparator '#(#t #t) '#(#t #t))))
		  (test-assert (not (<? default-comparator '#(#t #t) '#(#f #t))))
		  (test-assert (not (<? default-comparator '#(#f #t) '#(#f #f))))

		  (test-assert (= (comparator-hash default-comparator #t) (boolean-hash #t)))
		  (test-assert (= (comparator-hash default-comparator #\t) (char-hash #\t)))
		  (test-assert (= (comparator-hash default-comparator "t") (string-hash "t")))
		  (test-assert (= (comparator-hash default-comparator 't) (symbol-hash 't)))
		  (test-assert (= (comparator-hash default-comparator 10) (number-hash 10)))
		  (test-assert (= (comparator-hash default-comparator 10.0) (number-hash 10.0)))

		  (comparator-register-default!
		   (make-comparator procedure? (lambda (a b) #t) (lambda (a b) #f) (lambda (obj) 200)))
		  (test-assert (=? default-comparator (lambda () #t) (lambda () #f)))
		  (test-assert (not (<? default-comparator (lambda () #t) (lambda () #f))))
		  (test-eqv 200 (comparator-hash default-comparator (lambda () #t)))
		  )
	
      (test-group "Comparators/accessors"
		  (define ttp (lambda (x) #t))
		  (define eqp (lambda (x y) #t))
		  (define orp (lambda (x y) #t))
		  (define hf (lambda (x) 0))
		  (define comp (make-comparator ttp eqp orp hf))
		  (test-eq ttp (comparator-type-test-predicate comp))
		  (test-eq eqp (comparator-equality-predicate comp))
		  (test-eq orp (comparator-ordering-predicate comp))
		  (test-eq hf (comparator-hash-function comp))
		  )
					
      (test-group "Comparators/invokers"
		  (test-assert (comparator-test-type real-comparator 3))
		  (test-assert (comparator-test-type real-comparator 3.0))
		  (test-assert (not (comparator-test-type real-comparator "3.0")))
		  (test-assert (comparator-check-type boolean-comparator #t))
		  (test-error (comparator-check-type boolean-comparator 't))
		  )

      (test-group "Comparators/comparison"
		  (test-assert (=? real-comparator 2 2.0 2))
		  (test-assert (<? real-comparator 2 3.0 4))
		  (test-assert (>? real-comparator 4.0 3.0 2))
		  (test-assert (<=? real-comparator 2.0 2 3.0))
		  (test-assert (>=? real-comparator 3 3.0 2))
		  (test-assert (not (=? real-comparator 1 2 3)))
		  (test-assert (not (<? real-comparator 3 1 2)))
		  (test-assert (not (>? real-comparator 1 2 3)))
		  (test-assert (not (<=? real-comparator 4 3 3)))
		  (test-assert (not (>=? real-comparator 3 4 4.0)))
		  )

      (test-group "Comparators/syntax"
		  (test-eq 'less (comparator-if<=> real-comparator 1 2 'less 'equal 'greater))
		  (test-eq 'equal (comparator-if<=> real-comparator 1 1 'less 'equal 'greater))
		  (test-eq 'greater (comparator-if<=> real-comparator 2 1 'less 'equal 'greater))
		  (test-eq 'less (comparator-if<=> "1" "2" 'less 'equal 'greater))
		  (test-eq 'equal (comparator-if<=> "1" "1" 'less 'equal 'greater))
		  (test-eq 'greater (comparator-if<=> "2" "1" 'less 'equal 'greater))
		  )

      (test-group "Comparators/bound-salt"
		  (test-assert (exact-integer? (hash-bound)))
		  (test-assert (exact-integer? (hash-salt)))
		  (test-assert (< (hash-salt) (hash-bound)))
		  )

      (test-end))

    (define (vector-cdr vec)
      (let* ((len (vector-length vec))
	     (result (make-vector (- len 1))))
	(let loop ((n 1))
	  (cond
	   ((= n len) result)
	   (else (vector-set! result (- n 1) (vector-ref vec n))
		 (loop (+ n 1)))))))
    
    (define default-comparator (make-default-comparator))

    (define real-comparator (make-comparator real? = < number-hash))

    (define degenerate-comparator (make-comparator (lambda (x) #t) equal? #f #f))

    (define boolean-comparator
      (make-comparator boolean? eq? (lambda (x y) (and (not x) y)) boolean-hash))

    (define bool-pair-comparator (make-pair-comparator boolean-comparator boolean-comparator))

    (define num-list-comparator
      (make-list-comparator real-comparator list? null? car cdr))

    (define num-vector-comparator
      (make-vector-comparator real-comparator vector? vector-length vector-ref))

    (define vector-qua-list-comparator
      (make-list-comparator
       real-comparator
       vector?
       (lambda (vec) (= 0 (vector-length vec)))
       (lambda (vec) (vector-ref vec 0))
       vector-cdr))

    (define list-qua-vector-comparator
      (make-vector-comparator default-comparator list? length list-ref))

    (define eq-comparator (make-eq-comparator))

    (define eqv-comparator (make-eqv-comparator))

    (define equal-comparator (make-equal-comparator))

    (define symbol-comparator
      (make-comparator
       symbol?
       eq?
       (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
       symbol-hash))))
