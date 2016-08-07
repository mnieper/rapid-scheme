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

(define (make-vertex value)
  (%make-vertex value '() #f #f #f #f))

(define (%make-vertex value successors index lowlink on-stack? scc)
  (vector value successors index lowlink on-stack? scc))

(define (vertex-value vertex) (vector-ref vertex 0))

(define (vertex-successors vertex) (vector-ref vertex 1))

(define (vertex-set-successors! vertex successors) (vector-set! vertex 1 successors))

(define (vertex-index vertex) (vector-ref vertex 2))

(define (vertex-set-index! vertex index) (vector-set! vertex 2 index))

(define (vertex-lowlink vertex) (vector-ref vertex 3))

(define (vertex-set-lowlink! vertex lowlink) (vector-set! vertex 3 lowlink))

(define (vertex-on-stack? vertex) (vector-ref vertex 4))

(define (vertex-set-on-stack?! vertex flag) (vector-set! vertex 4 flag))

(define (vertex-scc vertex) (vector-ref vertex 5))

(define (vertex-set-scc! vertex scc) (vector-set! vertex 5 scc))

(define (graph-scc neighbors-alist vertex-comparator)

  (define equality (comparator-equality-predicate vertex-comparator))

  (define index 0)

  (define stack '())

  (define (push! vertex)
    (set! stack (cons vertex stack))
    (vertex-set-on-stack?! vertex #t))

  (define (pop!)
    (let ((vertex (car stack)))
      (set! stack (cdr stack))
      (vertex-set-on-stack?! vertex #f)
      vertex))  

  (define vertices (imap vertex-comparator))

  (define (intern-vertex! value)
    (cond
     ((imap-ref/default vertices value #f)
      => (lambda (vertex)
	   vertex))
     (else
      (let ((vertex (make-vertex value)))
	(set! vertices
	      (imap-replace vertices value vertex))
	vertex))))

  (define (ref-vertex vertex)
    (imap-ref vertices vertex))

  ;; XXX: Is a list-queue better here?
  (define sccs '())

  (define (strong-connect! vertex)
    (vertex-set-index! vertex index)
    (vertex-set-lowlink! vertex index)
    (set! index (+ index 1))
    (push! vertex)
    (for-each
     (lambda (successor)
       (cond
	((not (vertex-index successor))
	 (strong-connect! successor)
	 (vertex-set-lowlink! vertex (min (vertex-lowlink vertex)
					  (vertex-lowlink successor))))
	((vertex-on-stack? successor)
	 (vertex-set-lowlink! vertex (min (vertex-lowlink vertex)
					  (vertex-lowlink successor))))))
     (vertex-successors vertex))
    (when (= (vertex-lowlink vertex)
	     (vertex-index vertex))
      (let ((root (vertex-value vertex)) (scc (box '())))
	(let loop ()
	  (define successor (pop!))
	  (vertex-set-scc! successor scc)
	  (if (equality root (vertex-value successor))
	      (set! sccs (cons scc sccs))
	      (loop))))))
  
  ;; Intern vertices and edges
  (for-each
   (lambda (neighbors)
     (define vertex (intern-vertex! (car neighbors)))
     (vertex-set-successors! vertex (map intern-vertex! (cdr neighbors))))
   neighbors-alist)

  ;; Run Tarjan's algorithm
  (imap-for-each
   (lambda (value vertex)
     (unless (vertex-index vertex)
       (strong-connect! vertex)))
   vertices)

  ;; Distribute vertices on sccs in order
  (for-each
   (lambda (neighbors)
     (let* ((vertex (ref-vertex (car neighbors)))
	    (scc (vertex-scc vertex)))
       (set-box! scc (cons (vertex-value vertex) (unbox scc)))))
   neighbors-alist)

  ;; Return sccs
  (map
   (lambda (scc)
     (reverse (unbox scc)))
   sccs))
