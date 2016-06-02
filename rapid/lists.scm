;;; Rapid Scheme --- An implementation of basic Scheme libraries

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

;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

(define (find pred clist)
  (let loop ((clist clist))
    (and (not (null? clist))
	 (if (pred (car clist))
	     (car clist)
	     (loop (cdr clist))))))

(define (cons* element . element*)
  (let loop ((element element) (element* element*))
    (if (null? element*)
	element
	(cons element (loop (car element*) (cdr element*))))))

(define (take list k)
  (let loop ((list list) (k k))
    (if (zero? k)
	'()
	(cons (car list) (loop (cdr list) (- k 1))))))

(define (drop list k)
  (let loop ((list list) (k k))
    (if (zero? k)
	list
	(loop (cdr list) (- k 1)))))

(define (take-right list k)
  (let loop ((list list) (ref (drop list k)))
    (if (pair? ref)
	(loop (cdr list) (cdr ref))
	list)))

(define (drop-right list k)
  (let loop ((list list) (ref (drop list k)))
    (if (pair? ref)
	(cons (car list) (loop (cdr list) (cdr ref)))
	'())))

(define (split-at x k)
  (let loop ((x x) (k k))
    (if (zero? k)
	(values '() x)
	(let-values (((head tail) (loop (cdr x) (- k 1))))
	  (values (cons (car x) head) tail)))))

(define (circular-list? obj)
  (let loop ((fast obj) (slow obj))
    (and (pair? fast)
	 (let ((fast (cdr fast)))
	   (and (pair? fast)
		(let ((fast (cdr fast))
		      (slow (cdr slow)))
		  (or (eq? fast slow)
		      (loop fast slow))))))))

(define (any pred clist)
  (let loop ((clist clist))
    (if (null? clist)
	#f
	(or (pred (car clist)) (loop (cdr clist))))))

(define (every pred clist)
  (let loop ((clist clist))
    (if (null? clist)
	#t
	(and (pred (car clist)) (loop (cdr clist))))))

(define (map-in-order proc . list*)
  (let loop ((list* list*))
    (call-with-current-continuation
     (lambda (return)
       (let ((value
	      (apply proc
		     (map
		      (lambda (list)
			(if (null? list)
			    (return '())
			    (car list)))
		      list*))))
	 (cons value (loop (map cdr list*))))))))

(define unfold
  (case-lambda
   ((pred? proc step seed)
    (unfold pred? proc step seed (lambda (x) '())))
   ((pred? proc step seed tail-gen)
    (if (pred? seed)
	(tail-gen seed)
	(cons (proc seed)
	      (unfold pred? proc step (step seed) tail-gen))))))
