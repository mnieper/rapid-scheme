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

(define-record-type <map>
  (%make-map comparator items)
  map?
  (comparator map-key-comparator)
  (items map-items))

(define (map-item-comparator map)
  (set-element-comparator (map-items map)))

(define (map-empty-copy map)
  (%make-map (map-key-comparator map)
	     (set (set-comparator (map-items map)))))

(define-record-type <item>
  (make-item key value)
  item?
  (key %item-key)
  (value item-value))

(define (item-key item)
  (if (item? item)
      (%item-key item)
      item))

(define (make-item-comparator comparator)
  (let ((type-test-predicate (comparator-type-test-predicate comparator))
	(equality-predicate (comparator-equality-predicate comparator))
	(ordering-predicate (comparator-ordering-predicate comparator))
	(hash-function (comparator-hash-function comparator)))
    (make-comparator
     (lambda (obj)
       ((or (and (item? obj)
		 (type-test-predicate (item-key obj)))
	    (type-test-predicate obj))))
     (lambda (item1 item2)
       (equality-predicate (item-key item1)
			   (item-key item2)))
     (lambda (item1 item2)
       (ordering-predicate (item-key item1)
			   (item-key item2)))
     (lambda (item)
       (hash-function (item-key item))))))

;; Constructors

(define (make-map comparator . args)
  (map-unfold null?
	      (lambda (args)
		(values (car args)
			(cadr args)))
	      cddr
	      args
	      comparator))

(define (map-unfold stop? mapper successor seed comparator)
  (receive (items)
      (set-unfold stop?
		  (lambda (seed)
		    (receive (key value)
			(mapper seed)
		      (make-item key value)))
		  successor
		  seed
		  (make-item-comparator comparator))
    (%make-map comparator items)))

;; Predicates

(define (map-contains? map key)
  (set-contains? (map-items map) key))

(define (map-empty? map)
  (set-empty? (map-items map)))

(define (map-disjoint? map1 map2)
  (set-disjoint? (map-items map1)
		 (map-items map2)))

;; Accessors

(define map-ref
  (case-lambda
    ((map key)
     (map-ref map key (lambda ()
			 (error "map-ref: key not in map" key))))

    ((map key failure)
     (map-ref map key failure (lambda (value)
				value)))
    ((map key failure success)
     (cond
      ((set-member (map-items map) key #f)
       => (lambda (item)
	    (success (item-value item))))
      (else
       (failure))))))

(define (map-ref/default map key default)
  (map-ref map key (lambda ()
		      default)))

;; Updaters

(define (map-set! map . args)
   (let loop ((map map)
	     (args args))
    (if (null? args)
	map
	(loop (map-update! map
			   (car args)
			   (lambda (obj) (cadr args))
			   (lambda () #f))
	      (cddr args)))))

(define (map-set map . args)
   (let loop ((map map)
	     (args args))
    (if (null? args)
	map
	(loop (map-update map
			  (car args)
			  (lambda (obj) (cadr args))
			  (lambda () #f))
	      (cddr args)))))

(define (map-delete map . keys)
  (%make-map (map-key-comparator map) (set-delete-all (map-items map)
						      keys)))

(define (map-delete! map . keys)
  (%make-map (map-key-comparator map) (set-delete-all! (map-items map)
						       keys)))

(define (map-replace map key value)
  (%make-map (map-key-comparator key)
	     (set-replace (map-items map)
			  (make-item key value))))

(define (map-replace! map key value)
  (%make-map (map-key-comparator key)
	     (set-replace! (map-items map)
			   (make-item key value))))

(define (map-intern! map key failure)
  (call-with-current-continuation
   (lambda (return)
     (values map 
	     (map-ref map
		      key
		      (lambda ()
			(return (map-set! map key (failure)))))))))

(define (map-intern map key failure)
  (call-with-current-continuation
   (lambda (return)
     (values map 
	     (map-ref map
		      key
		      (lambda ()
			(return (map-set map key (failure)))))))))

(define map-update!
  (case-lambda
    ((map key updater)
     (map-update map key updater (lambda ()
				    (error "map-update!: key not in map" key))))
    ((map key updater failure)
     (map-update map key updater failure (lambda (value)
					    value)))
    ((map key updater failure success)
     (let ((items (map-items map)))
       (receive (items)
	   (call-with-current-continuation
	    (lambda (return)
	      (set-replace! items
			    (make-item key
				       (updater (map-ref map
							 key
							 (lambda ()
							   (return
							    (set-adjoin items
									(make-item
									 key
									 (updater (failure))))))
							 success))))))
	 (%make-map (map-key-comparator map) items))))))

(define map-update
  (case-lambda
    ((map key updater)
     (map-update map key updater (lambda ()
				    (error "map-update!: key not in map" key))))
    ((map key updater failure)
     (map-update map key updater failure (lambda (value)
					    value)))
    ((map key updater failure success)
     (let ((items (map-items map)))
       (receive (items)
	   (call-with-current-continuation
	    (lambda (return)
	      (set-replace items
			   (make-item key
				      (updater (map-ref map
							key
							(lambda ()
							  (return (set-adjoin
								   items
								   (make-item
								    key
								    (updater (failure))))))
							success))))))
	 (%make-map (map-key-comparator map) items))))))

(define (map-update!/default map key updater default)
  (map-update! map key updater (lambda ()
				 default)))

(define (map-update/default map key updater default)
  (map-update!/default (map-copy map) key updater default))

#;
(define (map-search! map key failure success)  
  (receive (items obj)
      (call-with-current-continuation
       (lambda (return)
	 (set-search! (map-items map)
		      key
		      (lambda (insert ignore)
			(failure (lambda (value obj)
				   (return (set-adjoin ))

				   
				   (insert (make-item key value) 
					   ))
				 ignore))
		      (lambda (item update remove)
			(success (item-key item)
				 (item-value item)
				 (lambda (key value obj)
				   (update (make-item key value) obj))
				 remove)))))
    (values (%make-map (map-key-comparator map)
 		       items)
	    obj)))

#;
(define (map-search map key failure success)
  (map-search! (map-copy map) key failure success))	       

;; The whole hash table

(define (map-size map)
  (set-size (map-items map)))

(define (map-find predicate map failure)
  (receive (item)
      (set-find (lambda (item)
		  (let ((key (item-key item))
			(value (item-value item)))
		    (predicate key value)))
		(map-items map)
		failure)
    (values (item-key item)
	    (item-value item))))

(define (map-keys map)
  (set-fold (lambda (item keys)
	      (cons (item-key item)
		    keys))
	    '()
	    (map-items map)))

(define (map-values map)
  (set-fold (lambda (item keys)
	      (cons (item-value item)
		    keys))
	    '()
	    (map-items map)))

(define (map-entries map)
  (receive (entries)
      (set-fold (lambda (item entries)
		  (list (cons (item-key item)
			      (list-ref entries 0))
			(cons (item-value item)
			      (list-ref entries 1))))
		(list '()
		      '())
		(map-items map))
    (apply values entries)))

(define (map-count predicate map)
  (set-count (lambda (item)
	       (predicate (item-key item)
			  (item-value item)))
	     (map-items map)))

(define (map-any? predicate map)
  (set-any? (lambda (item)
	      (predicate (item-key item)
			 (item-value item)))
	    (map-items map)))

(define (map-every? predicate map)
  (set-every? (lambda (item)
	      (predicate (item-key item)
			 (item-value item)))
	    (map-items map)))

;; Mapping and folding

(define (map-map proc comparator map)
  (%make-map comparator
	     (set-map (lambda (item)
			(proc (item-key item)
			      (item-value item)))
		      (make-item-comparator comparator)
		      (map-items map))))

(define (map-for-each proc map)
  (set-for-each (lambda (item)
		  (proc (item-key item)
			(item-value item)))
		(map-items map)))

(define (map-map->list proc map)
  (map-fold (lambda (key value list)
	      (cons (proc key value)
		    list))
	    '()
	    map))

(define (map-fold proc nil map)
  (set-fold (lambda (item result)
	      (proc (item-key item)
		    (item-value item)
		    result))
	    nil (map-items map)))

(define (map-filter predicate map)
  (%make-map (map-key-comparator map)	    
	     (set-filter (lambda (item)
			   (predicate (item-key item)
				      (item-value item)))
			 (map-items map))))

(define (map-filter! predicate map)
  (%make-map (map-key-comparator map)	    
	     (set-filter! (lambda (item)
			    (predicate (item-key item)
				       (item-value item)))
			  (map-items map))))

(define (map-remove predicate map)
  (%make-map (map-key-comparator map)	    
	     (set-remove (lambda (item)
			   (predicate (item-key item)
				      (item-value item)))
			 (map-items map))))

(define (map-remove! predicate map)
  (%make-map (map-key-comparator map)	    
	     (set-remove! (lambda (item)
			    (predicate (item-key item)
				       (item-value item)))
			  (map-items map))))

(define (map-partition predicate map)
  (let ((comparator (map-key-comparator map)))
    (receive (set1 set2)
	(set-partition (lambda (item)
			  (predicate (item-key item)
				     (item-value item)))
			map)
      (values (%make-map comparator set1)
	      (%make-map comparator set2)))))

(define (map-partition! predicate map)
  (let ((comparator (map-key-comparator map)))
    (receive (set1 set2)
	(set-partition! (lambda (item)
			  (predicate (item-key item)
				     (item-value item)))
			map)
      (values (%make-map comparator set1)
	      (%make-map comparator set2)))))

;; Copying and conversion

(define (map-copy map)
  (%make-map (map-key-comparator map) (set-copy (map-items map))))

(define (map-empty-copy map)
  (%make-map (map-key-comparator map)
	     (set (map-item-comparator map))))

(define (map->alist map)
  (map-map->list cons map))

(define (alist->map comparator alist)
  (map-unfold null?
	      (lambda (item)
		(values (car item)
			(cdr item)))
	      cdr
	      alist
	      comparator))

;; Set theory operations

(define (map-union maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-union (map map-items maps))))

(define (map-intersection maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-intersection (map map-items maps))))

(define (map-difference maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-difference (map map-items maps))))

(define (map-xor maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-xor (map map-items maps))))

(define (map-union! maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-union! (map map-items maps))))

(define (map-intersection! maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-intersection! (map map-items maps))))

(define (map-difference! maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-difference! (map map-items maps))))

(define (map-xor! maps)
  (%make-map (map-key-comparator (car maps))
	     (apply set-xor! (map map-items maps))))
