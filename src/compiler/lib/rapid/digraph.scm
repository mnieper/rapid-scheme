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


(define-record-type <digraph>
  (make-digraph nodes)
  digraph?
  (nodes digraph-nodes))

(define-record-type <digraph-node>
  (%make-node successors predecessors)
  node?
  (successors node-successors)
  (predecessors node-predecessors))

(define (make-node comparator)
  (%make-node (set comparator)
	      (set comparator)))

(define (node-adjoin-successor node . successors)
  (%make-node (apply set-adjoin (node-successors node) successors)
	      (node-predecessors node)))

(define (node-adjoin-predecessor node . predecessors)
  (%make-node (node-successors node)
	      (apply set-adjoin (node-predecessors node) predecessors)))

;; Constructors

(define (digraph comparator)
  (make-digraph (make-map comparator)))

;; Accessors

(define (digraph-node-comparator digraph)
  (map-key-comparator (digraph-nodes digraph)))

(define (digraph-node-set digraph)
  (map-fold (lambda (label node set)
	      (set-adjoin set label))
	    (set (digraph-node-comparator digraph)
		 (digraph-nodes digraph))))

(define (digraph-successor-set digraph label)
  (node-successors (map-ref (digraph-nodes digraph) label)))

(define (digraph-predecessor-set digraph label)
  (node-predecessors (map-ref (digraph-nodes digraph) label)))

;; Updaters

(define (digraph-adjoin-edge digraph source target)
  (let*
      ((comparator (digraph-node-comparator digraph))
       (node-maker (lambda ()
		     (make-node comparator))) 
       (nodes (digraph-nodes digraph))
       (nodes (map-update nodes source
			  (lambda (node)
			    (node-adjoin-successor node target))
			  node-maker))
       (nodes (map-update nodes target
			  (lambda (node)
			    (node-adjoin-predecessor node source))
			  node-maker)))
    (make-digraph nodes)))

(define (digraph-adjoin-node digraph label . targets)
  (let*
      ((comparator (digraph-node-comparator digraph))
       (node-maker (lambda ()
		      (make-node comparator)))      
       (nodes (digraph-nodes digraph))
       (nodes (map-update nodes label
			  (lambda (node)
			    (apply node-adjoin-successor node targets))
			  node-maker))
       (nodes (fold (lambda (target nodes)
		      (map-update nodes target
				  (lambda (node)
				    (node-adjoin-predecessor node label))
				  node-maker))	    
		    nodes targets)))
    (make-digraph nodes)))

;; The whole graph

(define (digraph-find predicate digraph failure)
  (receive (label node)
      (map-find (lambda (label node)
		  (predicate label
			     (node-successors node)
			     (node-predecessors node)))
		(digraph-nodes digraph)
		failure)
    label))

;; Mapping and folding

(define (digraph-fold proc nil digraph)
  (map-fold (lambda (label node result)
	      (proc label
		    (node-successors node)
		    (node-predecessors node)
		    result))
	    nil (digraph-nodes digraph)))

;; Copying and conversion

(define (digraph->alist digraph)
  (digraph-fold (lambda (label successors predecessors alist)
		  (cons (cons label (set->list successors))
			alist))
		'()
		digraph))

(define (alist->digraph comparator alist)
  (fold (lambda (nodes digraph)
	  (apply digraph-adjoin-node digraph nodes))
	(digraph comparator)
	alist))

;; Graph algorithms

(define (digraph-interval digraph start header)
  (let ((compare (comparator-equality-predicate (digraph-node-comparator digraph))))  
    (call-with-current-continuation
     (lambda (return)
       (let loop ((interval (set (digraph-node-comparator digraph) header)))       
	 (loop (set-adjoin interval
			   (digraph-find (lambda (label successors predecessors)
					   (and (not (compare start label))
						(not (set-contains? interval label))
						(set<=? predecessors interval)))
					 digraph
					 (lambda ()
					   (return interval))))))))))
