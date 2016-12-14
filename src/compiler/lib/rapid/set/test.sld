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


(define-library (rapid set test)
  (export run-tests)
  (import (scheme base)
	  (scheme char)
	  (rapid test)
	  (rapid comparator)
	  (rapid set))
  (begin
    (define number-comparator (make-default-comparator))
    (define char-comparator (make-default-comparator))
    (define eq-comparator (make-default-comparator))
    (define eqv-comparator (make-default-comparator))
    (define string-ci-comparator (make-comparator string? string-ci=? string-ci<? string-ci-hash))
    (define default-comparator (make-default-comparator))
    
    (define (run-tests)
      (test-begin "Sets and bags")

      (test-group "sets"
	(define (big x) (> x 5))
	
	(test-group "sets"
	  (test-group "sets/simple"
	    (define total 0)

	    (define nums (set number-comparator))
	    ;; nums is now {}
	    (define syms (set eq-comparator 'a 'b 'c 'd))
	    ;; syms is now {a, b, c, d}
	    (define nums2 (set-copy nums))
	    ;; nums2 is now {}
	    (define syms2 (set-copy syms))
	    ;; syms2 is now {a, b, c, d}
	    (define esyms (set eq-comparator))
	    ;; esyms is now {}

	    (test-assert (set-empty? esyms))
	    (test-assert (set? nums))
	    (test-assert (set? syms))
	    (test-assert (set? nums2))
	    (test-assert (set? syms2))
	    (test-assert (not (set? 'a)))
	    (set! nums (set-adjoin! nums 2))
	    (set! nums (set-adjoin! nums 3))
	    (set! nums (set-adjoin! nums 4))
	    (set! nums (set-adjoin! nums 4))
	    ;; nums is now {2, 3, 4}
	    (test-eq 4 (set-size (set-adjoin nums 5)))
	    (test-eq 3 (set-size nums))
	    (test-eq 3 (set-size (set-delete syms 'd)))
	    (test-eq 2 (set-size (set-delete-all syms '(c d))))
	    (test-eq 4 (set-size syms))
	    (set! syms (set-adjoin! syms 'e 'f))
	    ;; syms is now {a, b, c, d, e, f}
	    (set! syms (set-delete-all! syms '(e f)))
	    (test-eq 4 (set-size syms))
	    ;; syms is now {a, b, c, d}
	    (test-eq 0 (set-size nums2))
	    (test-eq 4 (set-size syms2))
	    (set! nums (set-delete! nums 2))
	    ;; nums is now {3, 4}
	    (test-eq 2 (set-size nums))
	    (set! nums (set-delete! nums 1))
	    (test-eq 2 (set-size nums))
	    (set! nums2 (set-map (lambda (x) (* 10 x)) number-comparator nums))
	    ;; nums2 is now {30, 40}
	    (test-assert (set-contains? nums2 30))
	    (test-assert (not (set-contains? nums2 3)))
	    (set-for-each (lambda (x) (set! total (+ total x))) nums2)
	    (test-eq 70 total)
	    (test-eq 10 (set-fold + 3 nums))
	    (set! nums (set eqv-comparator 10 20 30 40 50))
	    ;; nums is now {10, 20, 30, 40, 50}
	    (test-skip 1) ;; FIXME: --> mailing list XXX
	    (test-assert
		(set=? nums (set-unfold
			     (lambda (i) (= i 0))
			     (lambda (i) (* i 10))
			     (lambda (i) (- i 1))
			     5
			     eqv-comparator)))
	    (test-equal '(a) (set->list (set eq-comparator 'a)))
	    (set! syms2 (list->set eq-comparator '(e f)))
	    ;; syms2 is now {e, f}
	    (test-eq 2 (set-size syms2))
	    (test-assert (set-contains? syms2 'e))
	    (test-assert (set-contains? syms2 'f))
	    (set! syms2 (list->set! syms2 '(a b)))
	    (test-eq 4 (set-size syms2))
	    ) ; end sets/simple

	  (test-group "sets/search"
	    (define yam (set char-comparator #\y #\a #\m))
	    (define (failure/insert insert ignore)
	      (insert 1))
	    (define (failure/ignore insert ignore)
	      (ignore 2))
	    (define (success/update element update remove)
	      (update #\b 3))
	    (define (success/remove element update remove)
	      (remove 4))
	    (define yam! (set char-comparator #\y #\a #\m #\!))
	    (define bam (set char-comparator #\b #\a #\m))
	    (define ym (set char-comparator #\y #\m))
	    (define-values (set1 obj1)
	      (set-search! (set-copy yam) #\! failure/insert error))
	    (define-values (set2 obj2)
	      (set-search! (set-copy yam) #\! failure/ignore error))
	    (define-values (set3 obj3)
	      (set-search! (set-copy yam) #\y error success/update))
	    (define-values (set4 obj4)
	      (set-search! (set-copy yam) #\a error success/remove))
	    (test-assert (set=? yam! set1))
	    (test-eqv 1 obj1)
	    (test-assert (set=? yam set2))
	    (test-eqv 2 obj2)
	    (test-assert (set=? bam set3)) 
	    (test-eqv 3 obj3)
	    (test-assert (set=? ym set4))
	    (test-eqv 4 obj4) ; end sets/search

	  (test-group "sets/subsets"
	    (define set2 (set number-comparator 1 2))
	    (define other-set2 (set number-comparator 1 2))
	    (define set3 (set number-comparator 1 2 3))
	    (define set4 (set number-comparator 1 2 3 4))
	    (define setx (set number-comparator 10 20 30 40))
	    (test-assert (set=? set2 other-set2))
	    (test-assert (not (set=? set2 set3)))
	    (test-assert (not (set=? set2 set3 other-set2)))
	    (test-assert (set<? set2 set3 set4))
	    (test-assert (not (set<? set2 other-set2)))
	    (test-assert (set<=? set2 other-set2 set3))
	    (test-assert (not (set<=? set2 set3 other-set2)))
	    (test-assert (set>? set4 set3 set2))
	    (test-assert (not (set>? set2 other-set2)))
	    (test-assert (set>=? set3 other-set2 set2))
	    (test-assert (not (set>=? other-set2 set3 set2)))
	    ) ; end sets/subsets

	  (test-group "sets/ops"
	    ;; Potentially mutable
	    (define abcd (set eq-comparator 'a 'b 'c 'd))
	    (define efgh (set eq-comparator 'e 'f 'g 'h))
	    (define abgh (set eq-comparator 'a 'b 'g 'h))
	    ;; Never get a chance to be mutated
	    (define other-abcd (set eq-comparator 'a 'b 'c 'd))
	    (define other-efgh (set eq-comparator 'e 'f 'g 'h))
	    (define other-abgh (set eq-comparator 'a 'b 'g 'h))
	    (define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
	    (define none (set eq-comparator))
	    (define ab (set eq-comparator 'a 'b))
	    (define cd (set eq-comparator 'c 'd))
	    (define ef (set eq-comparator 'e 'f))
	    (define gh (set eq-comparator 'g 'h))
	    (define cdgh (set eq-comparator 'c 'd 'g 'h))
	    (define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
	    (define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))
	    (test-assert (set-disjoint? abcd efgh))
	    (test-assert (not (set-disjoint? abcd ab)))
	    #;
	    (parameterize ((current-test-comparator set=?))
	      (define efgh2 (set-copy efgh))
	      (define abcd2 (set-copy abcd))
	      (define abcd3 (set-copy abcd))
	      (define abcd4 (set-copy abcd))
	      (test-eq all (set-union abcd efgh))
	      (test-eq abcdgh (set-union abcd abgh))
	      (test-eq abefgh (set-union efgh abgh))
	      (set-union! efgh2 abgh)
	      (test-eq abefgh efgh2)
	      (test-eq none (set-intersection abcd efgh))
	      (set-intersection! abcd2 efgh)
	      (test-eq none abcd2)
	      (test-eq ab (set-intersection abcd abgh))
	      (test-eq ab (set-intersection abgh abcd))
	      (test-eq cd (set-difference abcd ab))
	      (test-eq abcd (set-difference abcd gh))
	      (test-eq none (set-difference abcd abcd))
	      (set-difference! abcd3 abcd)
	      (test-eq none abcd3)
	      (test-eq cdgh (set-xor abcd abgh))
	      (test-eq all (set-xor abcd efgh))
	      (test-eq none (set-xor abcd other-abcd))
	      ;; don't test-eq xor! effect
	      (test-eq none (set-xor! abcd4 other-abcd))
	      (test-eq "abcd smashed?" other-abcd abcd)
	      (test-eq "efgh smashed?" other-efgh efgh)
	      (test-eq "abgh smashed?" other-abgh abgh))
	    ) ; end sets/subsets

	  #;
	  (test-group "sets/mismatch"
	    (define nums (set number-comparator 1 2 3))
	    (define syms (set eq-comparator 'a 'b 'c))
	    (test-error (set=? nums syms))
	    (test-error (set<? nums syms))
	    (test-error (set<=? nums syms))
	    (test-error (set>? nums syms))
	    (test-error (set>=? nums syms))
	    (test-error (set-union nums syms))
	    (test-error (set-intersection nums syms))
	    (test-error (set-difference nums syms))
	    (test-error (set-xor nums syms))
	    (test-error (set-union! nums syms))
	    (test-error (set-intersection! nums syms))
	    (test-error (set-difference! nums syms))
	    (test-error (set-xor! nums syms))
	    ) ; end sets/mismatch

	  (test-group "sets/whole"
	    (define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
	    (define whole2 (set-copy whole))
	    (define whole3 (set-copy whole))
	    (define whole4 (set-copy whole))
	    (define bottom (set eqv-comparator 1 2 3 4 5))
	    (define top (set eqv-comparator 6 7 8 9 10))
	    (define hetero (set eqv-comparator 1 2 'a 3 4))
	    (define homo (set eqv-comparator 1 2 3 4 5))
	    (define-values (topx bottomx)
	      (set-partition big whole))
	    (set-partition! big whole4)
	    #;
	    (parameterize ((current-test-comparator set=?))
	      (test-eq top (set-filter big whole))
	      (test-eq bottom (set-remove big whole))
	      (set-filter! big whole2)
	      (test-assert (not (set-contains? whole2 1)))
	      (set-remove! big whole3)
	      (test-assert (not (set-contains? whole3 10)))
	      (test-eq top topx)
	      (test-eq bottom bottomx)
	      (test-eq top whole4))
	    (test-eq 5 (set-count big whole))
	    (test-eq 'a (set-find symbol? hetero (lambda () (error "wrong"))))
	    (test-error  (set-find symbol? homo (lambda () (error "wrong"))))
	    (test-assert (set-any? symbol? hetero))
	    (test-assert (set-any? number? hetero))
	    (test-assert (not (set-every? symbol? hetero)))
	    (test-assert (not (set-every? number? hetero)))
	    (test-assert (not (set-any? symbol? homo)))
	    (test-assert (set-every? number? homo))
	    ) ; end sets/whole

	  (test-group "sets/lowlevel"
	    (define bucket (set string-ci-comparator "abc" "def"))
	    (define nums (set number-comparator 1 2 3))
	    (define nums2 (set-replace nums 2.0))
	    (define sos
	      (set set-comparator
		   (set eqv-comparator 1 2)
		   (set eqv-comparator 1 2)))
	    (test-eq string-ci-comparator (set-element-comparator bucket))
	    (test-assert (set-contains? bucket "abc"))
	    (test-assert (set-contains? bucket "ABC"))
	    (test-equal "def" (set-member bucket "DEF" "fqz"))
	    (test-equal "fqz" (set-member bucket "lmn" "fqz"))
	    ;; nums is now {1, 2, 3}
	    ;; nums2 is now {1, 2.0, 3}
	    (test-assert (set-any? inexact? nums2))
	    (set! nums (set-replace! nums 2.0))
	    ;; nums is now {1, 2.0, 3}
	    (test-assert (set-any? inexact? nums))
	    (test-eq 1 (set-size sos))
	    ) ; end sets/lowlevel

	  ) ; end sets

	(test-group "bags"
	  (test-group "bags/simple"
	    (define total 0)
	    (define nums (bag number-comparator))
	    ;; nums is now {}
	    (define syms (bag eq-comparator 'a 'b 'c 'd))
	    ;; syms is now {a, b, c, d}
	    (define nums2 (bag-copy nums))
	    ;; nums2 is now {}
	    (define syms2 (bag-copy syms))
	    ;; syms2 is now {a, b, c, d}
	    (define esyms (bag eq-comparator))
	    ;; esyms is now {}
	    (test-assert (bag-empty? esyms))
	    (test-assert (bag? nums))
	    (test-assert (bag? syms))
	    (test-assert (bag? nums2))
	    (test-assert (bag? syms2))
	    (test-assert (not (bag? 'a)))
	    (set! nums (bag-adjoin! nums 2))
	    (set! nums (bag-adjoin! nums 3))
	    (set! nums (bag-adjoin! nums 4))
	    ;; nums is now {2, 3, 4}
	    (test-eqv 4 (bag-size (bag-adjoin nums 5)))
	    (test-eqv 3 (bag-size nums))
	    (test-eqv 3 (bag-size (bag-delete syms 'd)))
	    (test-eqv 2 (bag-size (bag-delete-all syms '(c d))))
	    (test-eqv 4 (bag-size syms))
	    (set! syms (bag-adjoin! syms 'e 'f))
	    ;; syms is now {a, b, c, d, e, f}
	    (test-eqv 4 (bag-size (bag-delete-all! syms '(e f))))
	    ;; syms is now {a, b, c, d}
	    (test-eqv 3 (bag-size nums))
	    (set! nums (bag-delete! nums 1))
	    (test-eqv 3 (bag-size nums))
	    (set! nums2 (bag-map (lambda (x) (* 10 x)) number-comparator nums))
	    ;; nums2 is now {20, 30, 40}
	    (test-assert (bag-contains? nums2 30))
	    (test-assert (not (bag-contains? nums2 3)))
	    (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
	    (test-eqv 90 total)
	    (test-eqv 12 (bag-fold + 3 nums))
	    (set! nums (bag eqv-comparator 10 20 30 40 50))
	    ;; nums is now {10, 20, 30, 40, 50}
	    #;  ; XXX -> mailing list
	    (test-assert
		(bag=? nums (bag-unfold
			     (lambda (i) (= i 0))
			     (lambda (i) (* i 10))
			     (lambda (i) (- i 1))
			     5
			     eqv-comparator)))
	    (test-equal '(a) (bag->list (bag eq-comparator 'a)))
	    (set! syms2 (list->bag eq-comparator '(e f)))
	    ;; syms2 is now {e, f}
	    (test-eqv 2 (bag-size syms2))
	    (test-assert (bag-contains? syms2 'e))
	    (test-assert (bag-contains? syms2 'f))
	    (set! syms2 (list->bag! syms2 '(e f)))
	    ;; syms2 is now {e, e, f, f}
	    (test-eq 4 (bag-size syms2))
	    ) ; end bags/simple

	  (test-group "bags/search"
	    (define yam (bag char-comparator #\y #\a #\m))
	    (define (failure/insert insert ignore)
	      (insert 1))
	    (define (failure/ignore insert ignore)
	      (ignore 2))
	    (define (success/update element update remove)
	      (update #\b 3))
	    (define (success/remove element update remove)
	      (remove 4))
	    (define yam! (bag char-comparator #\y #\a #\m #\!))
	    (define bam (bag char-comparator #\b #\a #\m))
	    (define ym (bag char-comparator #\y #\m))
	    (define-values (bag1 obj1)
	      (bag-search! (bag-copy yam) #\! failure/insert error))
	    (define-values (bag2 obj2)
	      (bag-search! (bag-copy yam) #\! failure/ignore error))
	    (define-values (bag3 obj3)
	      (bag-search! (bag-copy yam) #\y error success/update))
	    (define-values (bag4 obj4)
	      (bag-search! (bag-copy yam) #\a error success/remove))
	    (test-assert (bag=? yam! bag1))
	    (test-eqv 1 obj1)
	    (test-assert (bag=? yam bag2))
	    (test-eqv 2 obj2)
	    (test-assert (bag=? bam bag3))
	    (test-eqv 3 obj3)
	    (test-assert (bag=? ym bag4))
	    (test-eqv 4 obj4)
	    ) ; end bags/search

	  (test-group "bags/elemcount"
	    (define mybag (bag eqv-comparator 1 1 1 1 1 2 2))
	    (test-eqv 5 (bag-element-count mybag 1))
	    (test-eqv 0 (bag-element-count mybag 3))
	    ) ; end bags/elemcount

	  (test-group "bags/subbags"
	    (define bag2 (bag number-comparator 1 2))
	    (define other-bag2 (bag number-comparator 1 2))
	    (define bag3 (bag number-comparator 1 2 3))
	    (define bag4 (bag number-comparator 1 2 3 4))
	    (define bagx (bag number-comparator 10 20 30 40))
	    (test-assert (bag=? bag2 other-bag2))
	    (test-assert (not (bag=? bag2 bag3)))
	    (test-assert (not (bag=? bag2 bag3 other-bag2)))
	    (test-assert (bag<? bag2 bag3 bag4))
	    (test-assert (not (bag<? bag2 other-bag2)))
	    (test-assert (bag<=? bag2 other-bag2 bag3))
	    (test-assert (not (bag<=? bag2 bag3 other-bag2)))
	    (test-assert (bag>? bag4 bag3 bag2))
	    (test-assert (not (bag>? bag2 other-bag2)))
	    (test-assert (bag>=? bag3 other-bag2 bag2))
	    (test-assert (not (bag>=? other-bag2 bag3 bag2)))
	    ) ; end bags/subbags

	  (test-group "bags/multi"
	    (define one (bag eqv-comparator 10))
	    (define two (bag eqv-comparator 10 10))
	    (test-assert (not (bag=? one two)))
	    (test-assert (bag<? one two))
	    (test-assert (not (bag>? one two)))
	    (test-assert (bag<=? one two))
	    (test-assert (not (bag>? one two)))
	    (test-assert (bag=? two two))
	    (test-assert (not (bag<? two two)))
	    (test-assert (not (bag>? two two)))
	    (test-assert (bag<=? two two))
	    (test-assert (bag>=? two two))
	    (test-equal '((10 . 2))
		  (let ((result '()))
		    (bag-for-each-unique
		     (lambda (x y) (set! result (cons (cons x y) result)))
		     two)
		    result))
	    (test-eqv 25 (bag-fold + 5 two))
	    (test-eqv 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two))
	    ) ; end bags/multi

	  (test-group "bags/ops"
	    ;; Potentially mutable
	    (define abcd (bag eq-comparator 'a 'b 'c 'd))
	    (define efgh (bag eq-comparator 'e 'f 'g 'h))
	    (define abgh (bag eq-comparator 'a 'b 'g 'h))
	    ;; Never get a chance to be mutated
	    (define other-abcd (bag eq-comparator 'a 'b 'c 'd))
	    (define other-efgh (bag eq-comparator 'e 'f 'g 'h))
	    (define other-abgh (bag eq-comparator 'a 'b 'g 'h))
	    (define all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
	    (define none (bag eq-comparator))
	    (define ab (bag eq-comparator 'a 'b))
	    (define cd (bag eq-comparator 'c 'd))
	    (define ef (bag eq-comparator 'e 'f))
	    (define gh (bag eq-comparator 'g 'h))
	    (define cdgh (bag eq-comparator 'c 'd 'g 'h))
	    (define abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
	    (define abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))
	    (test-assert (bag-disjoint? abcd efgh))
	    (test-assert (not (bag-disjoint? abcd ab)))
	    #;
	    (parameterize ((current-test-comparator bag=?))
	      (define efgh2 (bag-copy efgh))
	      (define abcd2 (bag-copy abcd))
	      (define abcd3 (bag-copy abcd))
	      (define abcd4 (bag-copy abcd))
	      (define abab (bag eq-comparator 'a 'b 'a 'b))
	      (define ab2 (bag-copy ab))
	      (define ab3 (bag-copy ab))
	      (test-eq all (bag-union abcd efgh))
	      (test-eq abcdgh (bag-union abcd abgh))
	      (test-eq abefgh (bag-union efgh abgh))
	      (bag-union! efgh2 abgh)
	      (test-eq abefgh efgh2)
	      (test-eq none (bag-intersection abcd efgh))
	      (bag-intersection! abcd2 efgh)
	      (test-eq none abcd2)
	      (test-eq ab (bag-intersection abcd abgh))
	      (test-eq ab (bag-intersection abgh abcd))
	      (test-eq cd (bag-difference abcd ab))
	      (test-eq abcd (bag-difference abcd gh))
	      (test-eq none (bag-difference abcd abcd))
	      (bag-difference! abcd3 abcd)
	      (test-eq none abcd3)
	      (test-eq cdgh (bag-xor abcd abgh))
	      (test-eq all (bag-xor abcd efgh))
	      (test-eq none (bag-xor abcd other-abcd))
	      (test-eq none (bag-xor! abcd4 other-abcd))
	      (test-eq abab (bag-sum! ab2 ab))
	      (test-eq abab ab2)
	      (test-eq abab (bag-product 2 ab))
	      (bag-product! 2 ab3)
	      (test-eq abab ab3)
	      (test-eq "abcd smashed?" other-abcd abcd)
	      (test-eq "abcd smashed?" other-abcd abcd)
	      (test-eq "efgh smashed?" other-efgh efgh)
	      (test-eq "abgh smashed?" other-abgh abgh))
	    ) ; end bags/ops

	  #;
	  (test-group "bags/mismatch"
	    (define nums (bag number-comparator 1 2 3))
	    (define syms (bag eq-comparator 'a 'b 'c))
	    (test-error (bag=? nums syms))
	    (test-error (bag<? nums syms))
	    (test-error (bag<=? nums syms))
	    (test-error (bag>? nums syms))
	    (test-error (bag>=? nums syms))
	    (test-error (bag-union nums syms))
	    (test-error (bag-intersection nums syms))
	    (test-error (bag-difference nums syms))
	    (test-error (bag-xor nums syms))
	    (test-error (bag-union! nums syms))
	    (test-error (bag-intersection! nums syms))
	    (test-error (bag-difference! nums syms))
	    ) ; end bags/mismatch

	  (test-group "bags/whole"
	    (define whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
	    (define whole2 (bag-copy whole))
	    (define whole3 (bag-copy whole))
	    (define whole4 (bag-copy whole))
	    (define bottom (bag eqv-comparator 1 2 3 4 5))
	    (define top (bag eqv-comparator 6 7 8 9 10))
	    (define-values (topx bottomx)
	      (bag-partition big whole))
	    (define hetero (bag eqv-comparator 1 2 'a 3 4))
	    (define homo (bag eqv-comparator 1 2 3 4 5))
	    (bag-partition! big whole4)
	    #;
	    (parameterize ((current-test-comparator bag=?))
	      (test-eq top (bag-filter big whole))
	      (test-eq bottom (bag-remove big whole))
	      (bag-filter! big whole2)
	      (test-assert (not (bag-contains? whole2 1)))
	      (bag-remove! big whole3)
	      (test-assert (not (bag-contains? whole3 10)))
	      (test-eq top topx)
	      (test-eq bottom bottomx)
	      (test-eq top whole4))
	    (test-eqv 5 (bag-count big whole))
	    (test-eq 'a (bag-find symbol? hetero (lambda () (error "wrong"))))
	    (test-error  (bag-find symbol? homo (lambda () (error "wrong"))))
	    (test-assert (bag-any? symbol? hetero))
	    (test-assert (bag-any? number? hetero))
	    (test-assert (not (bag-every? symbol? hetero)))
	    (test-assert (not (bag-every? number? hetero)))
	    (test-assert (not (bag-any? symbol? homo)))
	    (test-assert (bag-every? number? homo))
	    ) ; end bags/whole

	  (test-group "bags/lowlevel"
	    (define bucket (bag string-ci-comparator "abc" "def"))
	    (define nums (bag number-comparator 1 2 3))
	    ;; nums is now {1, 2, 3}
	    (define nums2 (bag-replace nums 2.0))
	    ;; nums2 is now {1, 2.0, 3}
	    (define bob
	      (bag bag-comparator
		   (bag eqv-comparator 1 2)
		   (bag eqv-comparator 1 2)))
	    (test-eq string-ci-comparator (bag-element-comparator bucket))
	    (test-assert (bag-contains? bucket "abc"))
	    (test-assert (bag-contains? bucket "ABC"))
	    (test-equal "def" (bag-member bucket "DEF" "fqz"))
	    (test-equal "fqz" (bag-member bucket "lmn" "fqz"))
	    (test-assert (bag-any? inexact? nums2))
	    (set! nums (bag-replace! nums 2.0))
	    ;; nums is now {1, 2.0, 3}
	    (test-assert (bag-any? inexact? nums))
	    (test-eqv 2 (bag-size bob))
	    ) ; end bags/lowlevel


	  (test-group "bags/semantics"
	    (define mybag (bag number-comparator 1 2))
	    ;; mybag is {1, 2}
	    (test-eqv 2 (bag-size mybag))
	    (set! mybag (bag-adjoin! mybag 1))
	    ;; mybag is {1, 1, 2}
	    (test-eqv 3 (bag-size mybag))
	    (test-eqv 2 (bag-unique-size mybag))
	    (set! mybag (bag-delete! mybag 2))
	    ;; mybag is {1, 1}
	    (set! mybag (bag-delete! mybag 2))
	    (test-eqv 2 (bag-size mybag))
	    (set! mybag (bag-increment! mybag 1 3))
	    ;; mybag is {1, 1, 1, 1, 1}
	    (test-eqv 5 (bag-size mybag))
	    (set! mybag (bag-decrement! mybag 1 2))
	    (test-assert mybag)
	    ;; mybag is {1, 1, 1}
	    (test-eqv 3 (bag-size mybag))
	    (set! mybag (bag-decrement! mybag 1 5))
	    ;; mybag is {}
	    (test-eqv 0 (bag-size mybag))
	    ) ; end bags/semantics

	  (test-group "bags/convert"
	    (define multi (bag eqv-comparator 1 2 2 3 3 3))
	    (define single (bag eqv-comparator 1 2 3))
	    (define singleset (set eqv-comparator 1 2 3))
	    (define minibag (bag eqv-comparator 'a 'a))
	    (define alist '((a . 2)))
	    (test-equal alist (bag->alist minibag))
	    (test-assert (bag=? minibag (alist->bag eqv-comparator alist)))
	    (test-assert (set=? singleset (bag->set single)))
	    (test-assert (set=? singleset (bag->set multi)))
	    (test-assert (bag=? single (set->bag singleset)))
	    (test-assert (not (bag=? multi (set->bag singleset))))
	    (set! minibag (set->bag! minibag singleset))
	    ;; minibag is now {a, a, a, a, 1, 2, 3}
	    (test-assert (bag-contains? minibag 1))
	    ) ; end bags/convert

	  (test-group "bags/sumprod"
	    (define abb (bag eq-comparator 'a 'b 'b))
	    (define aab (bag eq-comparator 'a 'a 'b))
	    (define total (bag-sum abb aab))
	    (define bag1 (bag eqv-comparator 1))
	    (test-eqv 3 (bag-count (lambda (x) (eqv? x 'a)) total))
	    (test-eqv 3 (bag-count (lambda (x) (eqv? x 'b)) total))
	    (test-eqv 12 (bag-size (bag-product 2 total)))
	    (set! bag1 (bag-sum! bag1 bag1))
	    (test-eqv 2 (bag-size bag1))
	    (set! bag1 (bag-product! 2 bag1))
	    (test-eqv 4 (bag-size bag1))
	    ) ; end bag/sumprod

	  ) ; end bags
)

	(test-group "comparators"
	  (define a (set number-comparator 1 2 3))
	  (define b (set number-comparator 1 2 4))
	  (define aa (bag number-comparator 1 2 3))
	  (define bb (bag number-comparator 1 2 4))
	  (test-assert (not (=? set-comparator a b)))
	  (test-assert (=? set-comparator a (set-copy a)))
	  #;(test-error (<? set-comparator a b))
	  (test-assert (not (=? bag-comparator aa bb)))
	  (test-assert (=? bag-comparator aa (bag-copy aa)))
	  #;(test-error (<? bag-comparator aa bb))
	  (test-assert (not (=? default-comparator a aa)))
	  ) ; end comparators
)

	

      (test-end))))
