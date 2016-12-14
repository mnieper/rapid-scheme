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


(define-record-type <set-or-bag>
  (make-sob comparator tree multi?)
  sob?
  (comparator sob-comparator)
  (tree sob-tree)
  (multi? sob-multi?))

(define (set? obj) (and (sob? obj) (not (sob-multi? obj))))
(define (bag? obj) (and (sob? obj) (sob-multi? obj)))

(define (make-set comparator tree) (make-sob comparator tree #f))
(define (make-bag comparator tree) (make-sob comparator tree #t))

;; Constructors

(define (set comparator . elements)
  (unless (comparator? comparator)
    (error "???" comparator elements))
  
  (%sob comparator #f elements))
(define (bag comparator . elements)
  (%sob comparator #t elements))
(define (%sob comparator multi? elements)
  (sob-unfold comparator multi? null? car cdr elements))

(define (set-unfold comparator stop? mapper successor seed)
  (sob-unfold comparator #f stop? mapper successor seed))
(define (bag-unfold comparator stop? mapper successor seed)
  (sob-unfold comparator #t stop? mapper successor seed))
(define (sob-unfold comparator multi? stop? mapper successor seed)
  (let loop ((sob (make-sob comparator (make-tree) multi?))
	     (seed seed))
    (if (stop? seed)
	sob
	(loop (sob-adjoin sob (mapper seed))
	      (successor seed)))))

;; Predicates

(define (sob-contains? sob element)
  (call-with-current-continuation
   (lambda (return)
     (sob-search-unique sob
			element
			(lambda (insert ignore)
			  (return #f))
			(lambda (element count update remove)
			  (return #t))))))

(define set-contains? sob-contains?)
(define bag-contains? sob-contains?)

(define (sob-empty? sob)
  (not (sob-any? (lambda (obj) #t) sob)))

(define set-empty? sob-empty?)
(define bag-empty? sob-empty?)

(define (sob-disjoint? sob1 sob2)
  (call-with-current-continuation
   (lambda (return)
     (sob-for-each-unique (lambda (element count)
			    (when (sob-contains? sob2 element)
			      (return #f)))
			  sob1)
     #t)))

(define set-disjoint? sob-disjoint?)
(define bag-disjoint? sob-disjoint?)

;;; Accessors

(define (sob-member sob element default)
  (call-with-current-continuation
   (lambda (return)
     (sob-search-unique sob
			element
			(lambda (insert ignore)
			  (return default))
			(lambda (element count update remove)
			  (return element))))))

(define set-member sob-member)
(define bag-member sob-member)

(define set-element-comparator sob-comparator)
(define bag-element-comparator sob-comparator)

;;; Updaters

(define (sob-adjoin sob . elements)
  (fold (lambda (element sob)
	  (sob-increment sob element 1))
	sob elements))

(define set-adjoin sob-adjoin)
(define set-adjoin! set-adjoin)
(define bag-adjoin sob-adjoin)
(define bag-adjoin! sob-adjoin)

(define (sob-replace sob element)
  (receive (sob obj)
      (sob-search-unique sob
			 element
			 (lambda (insert ignore)
			   (ignore #f))
			 (lambda (old-element old-value update remove)
			   (update element old-value #f)))
    sob))

(define set-replace sob-replace)
(define set-replace! set-replace)
(define bag-replace sob-replace)
(define bag-replace! sob-replace)

(define (sob-delete sob . elements)
  (sob-delete-all sob elements))

(define set-delete sob-delete)
(define bag-delete sob-delete)
(define set-delete! sob-delete)
(define bag-delete! sob-delete)

(define (sob-delete-all sob elements)
  (fold (lambda (element sob)
	  (receive (sob obj)
	      (sob-search-unique sob
				 element
				 (lambda (insert ignore)
				   (ignore #f))
				 (lambda (old-element old-value update remove)
				   (remove #f)))
	    sob))
	sob elements))

(define set-delete-all sob-delete-all)
(define set-delete-all! sob-delete-all)
(define bag-delete-all sob-delete-all)
(define bag-delete-all! sob-delete-all)

(define (sob-search sob element failure success)
  (call-with-current-continuation
   (lambda (return)
     (sob-search-unique sob
			element
			(lambda (insert ignore)
			  (failure (lambda (obj)
				     (insert element 1 obj))
				   ignore))
			(lambda (old-element old-count update remove)
			  (success old-element
				   (lambda (new-element obj)
				     (unless (=? (sob-comparator sob) old-element new-element)
				       (return (sob-adjoin (sob-delete sob old-element)
							   new-element)
					       obj))
				     (update new-element old-count obj))
				   (lambda (obj)
				     (let ((new-count (- old-count 1)))
				       (if (zero? new-count)
					   (remove obj)
					   (update old-element new-count obj))))))))))
(define set-search! sob-search)
(define bag-search! sob-search)

(define (sob-search-unique sob element failure success)
  (let ((comparator (sob-comparator sob)))
    (receive (tree obj)
	(tree-search comparator
		     (sob-tree sob)
		     element
		     failure
		     success)
      (values (make-sob comparator tree (sob-multi? sob))
	      obj))))

;; The whole set/bag

(define (sob-size sob)
  (sob-fold-unique (lambda (element count size)
		     (+ size count))
		   0 sob))
(define set-size sob-size)
(define bag-size sob-size)

(define (sob-find predicate sob failure)
  (call-with-current-continuation
   (lambda (return)
     (sob-for-each (lambda (element)
		     (when (predicate element)
			(return element)))
		   sob)
     (failure))))
(define set-find sob-find)
(define bag-find sob-find)

(define (sob-count predicate sob)
  (sob-fold-unique (lambda (element count counter)
		     (if (predicate element)
			 (+ count counter)
			 counter))
		   0 sob))
(define set-count sob-count)
(define bag-count sob-count)

(define (sob-any? predicate sob)
  (call-with-current-continuation
   (lambda (return)
     (sob-for-each (lambda (element)
		     (when (predicate element)
		       (return #t)))
		   sob)
     #f)))
(define set-any? sob-any?)
(define bag-any? sob-any?)

(define (sob-every? predicate sob)
  (call-with-current-continuation
   (lambda (return)
     (sob-for-each (lambda (element)
		     (unless (predicate element)
		       (return #f)))
		   sob)
     #t)))
(define set-every? sob-every?)
(define bag-every? sob-every?)

;; Mapping and folding

(define (sob-map comparator proc sob)
  ;; XXX: See question on mailing list
  (if (comparator? proc)
      (sob-map proc comparator sob)      
      (let ((result (make-sob comparator (make-tree) (sob-multi? sob))))
	(sob-fold-unique (lambda (element count result)
			   (sob-increment result (proc element) count))
			 result sob))))
(define set-map sob-map)
(define bag-map sob-map)

(define (sob-for-each proc sob)
  (tree-for-each (lambda (key value)
		   (do ((value value (- value 1)))
		       ((zero? value))
		     (proc key)))
		 (sob-tree sob)))
(define set-for-each sob-for-each)
(define bag-for-each sob-for-each)

(define (sob-fold proc nil sob)
  (sob-fold-unique (lambda (element count nil)
		     (let loop ((count count) (nil nil))
		       (if (zero? count)
			   nil
			   (loop (- count 1) (proc element nil)))))
		   nil sob))
(define set-fold sob-fold)
(define bag-fold sob-fold)

(define (sob-filter predicate sob)
  (sob-fold-unique (lambda (element count sob)
		     (if (predicate element)
			 (sob-increment sob element count)
			 sob))
		   (make-sob (sob-comparator sob) (make-tree) (sob-multi? sob))))

(define set-filter sob-filter)
(define set-filter! set-filter)
(define bag-filter sob-filter)
(define bag-filter! bag-filter)

(define (sob-remove predicate sob)
  (sob-filter (lambda (obj) (not (predicate obj))) sob))
(define set-remove sob-remove)
(define set-remove! sob-remove)
(define bag-remove sob-remove)
(define bag-remove! sob-remove)


(define (sob-partition predicate sob)
  (let ((comparator (sob-comparator sob))
	(multi? (sob-multi? sob)))
    (receive (sob-pair)
	(sob-fold-unique (lambda (element count sob-pair)
			   (if (predicate element)
			       (cons (sob-increment (car sob-pair) element count)
				     (cdr sob-pair))
			       (cons (car sob-pair)
				     (sob-increment (cdr sob-pair) element count))))
			 (cons (make-sob comparator (make-tree) multi?)
			       (make-sob comparator (make-tree) multi?))
			 sob)
      (values (car sob-pair)
	      (cdr sob-pair)))))

(define set-partition sob-partition)
(define set-partition! sob-partition)
(define bag-partition sob-partition)
(define bag-partition! sob-partition)

;; Copying and conversion

(define (sob-copy sob) sob)
(define set-copy sob-copy)
(define bag-copy sob-copy)

(define (sob->list sob)
  (sob-fold cons '() sob))
(define set->list sob->list)
(define bag->list sob->list)

(define (list->set comparator list)
  (apply set comparator list))
(define (list->set! set list)
  (apply set-adjoin set list))
(define (list->bag comparator list)
  (apply bag comparator list))
(define (list->bag! bag list)
  (apply bag-adjoin bag list))

;; Subsets

(define sob=?
  (case-lambda
    ((sob) #t)
    ((sob1 sob2) (%sob=? sob1 sob2))
    ((sob1 sob2 . sobs)
     (and (%sob=? sob1 sob2)
          (apply sob=? sob2 sobs)))))
(define (%sob=? sob1 sob2)
  (and (%sob<=? sob1 sob2)
       (%sob<=? sob2 sob1)))
(define set=? sob=?)
(define bag=? sob=?)

(define sob<=?
  (case-lambda
    ((sob) #t)
    ((sob1 sob2) (%sob<=? sob1 sob2))
    ((sob1 sob2 . sobs)
     (and (%sob<=? sob1 sob2)
          (apply sob<=? sob2 sobs)))))
(define set<=? sob<=?)
(define bag<=? sob<=?)

(define (%sob<=? sob1 sob2)
  (call-with-current-continuation
   (lambda (return)
     (sob-for-each-unique (lambda (element count)
			    (unless (<= count (sob-element-count sob2 element))
			      (return #f)))
			  sob1)
     #t)))

(define sob>?
  (case-lambda
    ((sob) #t)
    ((sob1 sob2) (%sob>? sob1 sob2))
    ((sob1 sob2 . sobs)
     (and (%sob>? sob1 sob2)
          (apply sob>? sob2 sobs)))))
(define set>? sob>?)
(define bag>? sob>?)

(define (%sob>? sob1 sob2)
  (not (%sob<=? sob1 sob2)))

(define sob<?
  (case-lambda
    ((sob) #t)
    ((sob1 sob2) (%sob<? sob1 sob2))
    ((sob1 sob2 . sobs)
     (and (%sob<? sob1 sob2)
          (apply sob<? sob2 sobs)))))
(define set<? sob<?)
(define bag<? sob<?)

(define (%sob<? sob1 sob2)
  (%sob>? sob2 sob1))

(define sob>=?
  (case-lambda
    ((sob) #t)
    ((sob1 sob2) (%sob>=? sob1 sob2))
    ((sob1 sob2 . sobs)
     (and (%sob>=? sob1 sob2)
          (apply sob>=? sob2 sobs)))))
(define set>=? sob>=?)
(define bag>=? sob>=?)

(define (%sob>=? sob1 sob2)
  (not (%sob<? sob1 sob2)))

;; Set theory operations

(define-syntax define-operation*
  (syntax-rules ()
    ((define-operation* operation ... reducer)
     (begin
       (define r reducer)
       (define (%sob-operation sob1 sob2)
	 (sob-fold-unique (lambda (element count sob)
			    (receive (sob obj)
				(sob-search-unique sob
						   element
						   (lambda (insert ignore)
						     (let ((new-count (r 0 count)))
						       (if (zero? new-count)
							   (ignore #f)
							   (insert element new-count #f))))
						   (lambda (element1 count1 update remove)
						     (let ((new-count (r count1 count)))
						       (if (zero? new-count)
							   (remove #f)
							   (update element1 new-count #f)))))
			      sob))
			  sob1 sob2))
       (define sob-operation
	 (case-lambda
	   ((sob) sob)
	   ((sob1 sob2) (%sob-operation sob1 sob2))
	   ((sob1 sob2 . sobs)
	    ((apply sob-operation (%sob-operation sob1 sob2) sobs)))))
       (define operation sob-operation)
       ...))))

(define-operation* set-union set-union! bag-union bag-union! max)
(define-operation* bag-sum bag-sum! +)
(define-operation* set-intersection set-intersection! bag-intersection bag-intersection! min)
(define-operation* set-difference set-difference! bag-difference bag-difference!
  (lambda (x y)
    (max (- x y) 0)))
(define-operation* set-xor set-xor! bag-xor bag-xor!
  (lambda (x y)
    (abs (- x y))))

;;; Bag procedures

;; Additional bag procedures

(define (bag-product n sob)
  (bag-fold-unique (lambda (element count result)
		     (sob-increment result element (* count n)))
		   (bag (sob-comparator sob)) sob))
(define bag-product! bag-product)

(define (bag-unique-size bag)
  (bag-fold-unique (lambda (element count result)
		     (+ 1 result))
		   0 bag))

(define (sob-element-count sob element)
  (call-with-current-continuation
   (lambda (return)
     (tree-search (sob-comparator sob)
		  (sob-tree sob)
		  element
		  (lambda (insert ignore)
		    (return 0))
		  (lambda (key value update remove)
		    (return value))))))
(define bag-element-count sob-element-count)

(define (sob-for-each-unique proc sob)
  (tree-for-each proc (sob-tree sob)))
(define bag-for-each-unique sob-for-each-unique)

(define (sob-fold-unique proc acc sob)
  (tree-fold proc acc (sob-tree sob)))
(define bag-fold-unique sob-fold-unique)

(define (sob-increment sob element count)
  (receive (sob obj)
      (sob-search-unique sob
			 element
			 (lambda (insert ignore)
			   (insert element count #f))
			 (lambda (old-element old-count update remove)
			   (receive (new-count)
			       (if (sob-multi? sob)
				   (+ old-count count)
				   1)
			     (update old-element new-count #f))))
    sob))
(define bag-increment! sob-increment)

(define (sob-decrement sob element count)
  (receive (sob obj)
      (sob-search-unique sob
			 element
			 (lambda (insert ignore)
			   (ignore #f))
			 (lambda (old-element old-count update remove)
			   (let ((new-count (- old-count count)))
			     (if (<= new-count 0)
				 (remove #f)
				 (update element new-count #f)))))
    sob))
(define bag-decrement! sob-decrement)

(define (bag->set bag)
  (bag-fold-unique (lambda (element count set)
		     (set-adjoin set element))
		   (set (sob-comparator bag)) bag))

(define (set->bag set)
  (make-sob (sob-comparator set) (sob-tree set) #t))

(define (set->bag! bag set)
  (set-fold (lambda (element bag)
	      (bag-adjoin bag element))
	    bag set))

(define (bag->alist bag)
  (bag-fold-unique (lambda (element count alist)
		     (cons (cons element count) alist))
		   '() bag))

(define (alist->bag comparator alist)
  (fold (lambda (pair bag)
	  (bag-increment! bag (car pair) (cdr pair)))
	(bag comparator) alist))

;;; Comparators

(define (sob-hash sob)
  (let ((hash (comparator-hash-function (sob-comparator sob))))
    (sob-fold (lambda (element result)
		(+ (hash element) (* result 33)))
	      5381 sob)))

(define (sob-ordering sob1 sob2)
  (let* ((comparator (sob-comparator sob1))
	 (equality (comparator-equality-predicate comparator))
	 (ordering (comparator-ordering-predicate comparator))
	 (gen1 (tree-generator (sob-tree sob1)))
	 (gen2 (tree-generator (sob-tree sob2))))
    (let loop ()
      (let ((item1 (gen1)) (item2 (gen2)))
      	(cond
	 ((eof-object? item1)
	  (not (eof-object? item2)))
	 ((eof-object? item2)
	  #f)
	 (else
	  (let ((el1 (car item1)) (cnt1 (cadr item1))
		(el2 (car item2)) (cnt2 (cadr item2)))
	    (if (equality el1 el2
			  (and (<= el1 el2)
			       (or (< el1 el2)
				   (loop))))
		(ordering el1 el2)))))))))

(define set-comparator (make-comparator set? set=? sob-ordering sob-hash))
(define bag-comparator (make-comparator bag? bag=? sob-ordering sob-hash))
(comparator-register-default! set-comparator)
(comparator-register-default! bag-comparator)
