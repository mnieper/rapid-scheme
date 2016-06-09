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

;;; The exported concrete data type

(define-record-type <imap>
  (%imap comparator tree)
  imap?
  (comparator imap-comparator)
  (tree imap-tree))

;;; Internal concrete data types

(define (make-item key value) (vector key value))
(define (item-key item) (vector-ref item 0))
(define (item-value item) (vector-ref item 1))

(define (node color left item right) (vector color left item right))
(define (color node) (vector-ref node 0))
(define (left node) (vector-ref node 1))
(define (item node) (vector-ref node 2))
(define (right node) (vector-ref node 3))
(define (key node) (item-key (item node)))
(define (value node) (item-value (item node)))
(define (red left item right) (node 'red left item right))
(define black
  (case-lambda
   (() (black #f #f #f))
   ((left item right) (node 'black left item right))))
(define white
  (case-lambda
   (() (white #f #f #f))
   ((left item right) (node 'white left item right))))
(define (red? node) (eq? (color node) 'red))
(define (black? node) (eq? (color node) 'black))
(define (white? node) (eq? (color node) 'white))

;;; Tree matcher macros

(define-syntax tree-match
  (syntax-rules ()
    ((tree-match tree (pattern . expression*) ...)
     (compile-patterns (expression* ...) tree () (pattern ...)))))
     
(define-syntax compile-patterns
  (syntax-rules ()

    ((compile-patterns (expression* ...) tree (clauses ...) ())
     (call-with-current-continuation
      (lambda (return)
	(or (and-let* clauses (return (begin . expression*)))
	    ...
	    (error "tree does not match any pattern" tree)))))

    ((compile-patterns e tree clauses* (pattern . pattern*))
     (compile-pattern tree pattern
		      (add-pattern e tree clauses* pattern*)))))

(define-syntax add-pattern
  (syntax-rules ()
    ((add-pattern e tree (clauses ...) pattern* new-clauses)
     (compile-patterns e tree (clauses ... new-clauses) pattern*))))

(define-syntax compile-pattern
  (syntax-rules (_ and red? black? white? ? node red black white)

    ((compile-pattern tree (red? x) (k ...))
     (k ... (((red? tree)) (x tree))))

    ((compile-pattern tree (black? x) (k ...))
     (k ... (((black? tree)) (x tree))))

    ((compile-pattern tree (white? x) (k ...))
     (k ... (((white? tree)) (x tree))))

    ((compile-pattern tree (black) (k ...))
     (k ... (((black? tree)) ((not (item tree))))))

    ((compile-pattern tree (white) (k ...))
     (k ... (((white? tree)) ((not (item tree))))))

    ((compile-pattern tree (and pt ...) k*)
     (compile-subpatterns () ((t pt) ...)
			  (compile-and-pattern tree t k*)))

    ((compile-pattern tree (node pc pa px pb) k*)
     (compile-subpatterns () ((c pc) (a pa) (x px) (b pb))
			  (compile-node-pattern tree c a x b k*)))

    ((compile-pattern tree (red pa px pb) k*)
     (compile-subpatterns () ((a pa) (x px) (b pb))
			  (compile-color-pattern red? tree a x b k*)))

    ((compile-pattern tree (black pa px pb) k*)
     (compile-subpatterns () ((a pa) (x px) (b pb))
			  (compile-color-pattern black? tree a x b k*)))

    ((compile-pattern tree (white pa px pb) k*)
     (compile-subpatterns () ((a pa) (x px) (b pb))
			  (compile-color-pattern white? tree a x b k*)))

    ((compile-pattern tree _ (k ...))
     (k ... ()))

    ((compile-pattern tree x (k ...))
     (k ... ((x tree))))))

(define-syntax compile-and-pattern
  (syntax-rules ()    
    ((compile-and-pattern tree t (k ...) clauses)
     (k ... ((t tree) . clauses)))))

(define-syntax compile-node-pattern
  (syntax-rules ()
    ((compile-node-pattern tree c a x b (k ...) clauses)
     (k ... (((item tree))
	     (c (color tree))
	     (a (left tree))
	     (x (item tree))
	     (b (right tree)) . clauses)))))

(define-syntax compile-color-pattern
  (syntax-rules ()
    ((compile-color-pattern pred? tree a x b (k ...) clauses)
     (k ... (((item tree))
	     ((pred? tree))
	     (a (left tree))
	     (x (item tree))
	     (b (right tree)) . clauses)))))

(define-syntax compile-subpatterns
  (syntax-rules ()
    
    ((compile-subpatterns clauses () (k ...))
     (k ... clauses))

    ((compile-subpatterns clauses ((tree pattern) . rest) k*)
     (compile-pattern tree pattern (add-subpattern clauses rest k*)))))

(define-syntax add-subpattern
  (syntax-rules ()
    ((add-subpattern (clause ...) rest k* clauses)
     (compile-subpatterns (clause ... . clauses) rest k*))))
    
;;; Tree recolouring procedures

(define (blacken tree)
  (tree-match tree
    ((red a x b)
     (black a x b))
    (t t)))

(define (redden tree)
  (tree-match tree
    ((black (black? a) x (black? b))
     (red a x b))
    (t t)))

(define (white->black tree)
  (tree-match tree
    ((white)
     (black))
    ((white a x b)
     (black a x b))))

;;; Predicates for trees

(define (find? comparator tree obj)
  (let find? ((tree tree))
    (tree-match tree
      ((black)
       #f)
      ((node _ a x b)
       (comparator-if<=> comparator obj (item-key x)
	 (find? a)
	 x
	 (find? b))))))

(define (empty? tree)
  (tree-match tree
    ((black)
     #t)
    (_
     #f)))

(define (contains? comparator tree obj)
  (or (find? comparator tree obj)
      #f))

(define (ref comparator tree obj failure)
  (cond
   ((find? comparator tree obj)
    => item-value)
   (else
    (failure))))

;;; Update procedures for trees

(define (search comparator tree obj failure success)
  (let-values
      (((tree ret op)
	(let search ((tree (redden tree)))
	  (tree-match tree
	    
	    ((black)
	     (failure
	      ;; insert
	      (lambda (new-value ret)
		(values (red (black) (make-item obj new-value) (black))
			ret
			balance))
	      ;; ignore
	      (lambda (ret)
		(values (black) ret identity))))
	    
	    ((and t (node c a x b))
	     (let ((key (item-key x)))
	       (comparator-if<=> comparator obj key
		 
		 (let-values
		     (((a ret op) (search a)))
		   (values (op (node c a x b)) ret op))

		 (success
		  key
		  ;; update
		  (lambda (new-value ret)
		    (values (node c a (make-item obj new-value) b)
			    ret
			    identity))
		  ;; remove
		  (lambda (ret)
		    (values
		     (tree-match t
		       ((red (black) x (black))
			(black))
		       ((black (red a x b) _ (black))
			(black a x b))
		       ((black (black) _ (black))
			(white))
		       (_
			(let-values (((x b) (min+delete b)))
			  (rotate (node c a x b)))))
		     ret
		     rotate)))

		 (let-values
		     (((b ret op) (search b)))
		   (values (op (node c a x b)) ret op)))))))))
    (values (blacken tree) ret)))

(define (replace comparator tree key value)
  (let-values
      (((tree ret)
	(search
	 comparator tree key
	 (lambda (insert ignore)
	   (insert value #f))
	 (lambda (key update remove)
	   (update value #f)))))
    tree))

;;; Helper procedures for deleting and balancing

(define (min+delete tree)
  (tree-match tree
    ((red (black) x (black))
     (values x (black)))
    ((black (black) x (black))
     (values x (white)))
    ((black (black) x (red a y b))
     (values x (black a y b)))
    ((node c a x b)
     (let-values (((v a) (min+delete a)))
       (values v (rotate (node c a x b)))))))

(define (balance tree)
  (tree-match tree
    ((black (red (red a x b) y c) z d)
     (red (black a x b) y (black c z d)))
    ((black (red a x (red b y c)) z d)
     (red (black a x b) y (black c z d)))
    ((black a x (red (red b y c) z d))
     (red (black a x b) y (black c z d)))
    ((black a x (red b y (red c z d)))
     (red (black a x b) y (black c z d)))
    ((white (red a x (red b y c)) z d)
     (black (black a x b) y (black c z d)))
    ((white a x (red (red b y c) z d))
     (black (black a x b) y (black c z d)))   
    (t t)))

(define (rotate tree)
  (tree-match tree
    ((red (white? a+x+b) y (black c z d))
     (balance (black (red (white->black a+x+b) y c) z d)))
    ((red (black a x b) y (white? c+z+d))
     (balance (black a x (red b y (white->black c+z+d)))))
    ((black (white? a+x+b) y (black c z d))
     (balance (white (red (white->black a+x+b) y c) z d)))
    ((black (black a x b) y (white? c+z+d))
     (balance (white a x (red b y (white->black c+z+d)))))
    ((black (white? a+w+b) x (red (black c y d) z e))
     (black (balance (black (red (white->black a+w+b) x c) y d)) z e))
    ((black (red a w (black b x c)) y (white? d+z+e))
     (black a w (balance (black b x (red c y (white->black d+z+e))))))
    (t t)))

;;; Miscellaneous definitions

(define (identity x) x)

(define key-not-found (string-copy "key not found"))

(define (raise-key-not-found key)
  (error key-not-found key))

;;; Exported procedures

(define (imap-key-not-found? obj)
  (and (error-object? obj) (eq? (error-object-message obj) key-not-found)))

(define (imap comparator . associations)
  (%imap comparator  
	 (let loop ((associations associations))
	   (if (null? associations)
	       (black)
	       (replace comparator
			(loop (cddr associations))
			(car associations)
			(cadr associations))))))

(define (imap-contains? map obj)
  (contains? (imap-comparator map) (imap-tree map) obj))

(define (imap-empty? map)
  (empty? (imap-tree map)))

(define imap-ref
  (case-lambda
   ((map key)
    (imap-ref map key (lambda ()
			(raise-key-not-found key))))
   ((map key failure)
    (ref (imap-comparator map) (imap-tree map) key failure))))

(define (imap-ref/default map key default)
  (imap-ref map key (lambda () default)))

(define (imap-replace map key value)
  (let
      ((comparator
	(imap-comparator map))
       (tree
	(imap-tree map)))
    (%imap comparator (replace comparator tree key value))))

(define (imap-search map obj failure success)
  (let*-values
      (((comparator)
	(imap-comparator map))
       ((tree ret)
	(search comparator (imap-tree map) obj failure success)))
    (values (%imap comparator tree) ret)))

(define (imap-delete map key)
  (let-values
      (((map ret)
	(imap-search map key
		     (lambda (insert ignore)
		       (ignore #f))
		     (lambda (key update remove)
		       (remove #f)))))
    map))

;; Local Variables:
;; eval: (put 'tree-match 'scheme-indent-function 1)
;; eval: (put 'comparator-if<=> 'scheme-indent-function 3)
;; End:
