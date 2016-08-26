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

(define-record-type <list-queue>
  (%make-list-queue first last)
  list-queue?
  (first list-queue-list list-queue-set-first!)
  (last list-queue-last list-queue-set-last!))

(define (list-queue)
  (%make-list-queue '() '()))

(define (list-queue-empty? list-queue)
  (null? (list-queue-list list-queue)))

(define (list-queue-first-last list-queue)
  (values (list-queue-list list-queue)
	  (list-queue-last list-queue)))

(define (list-queue-front list-queue)
  (car (list-queue-list list-queue)))

(define (list-queue-add-back! list-queue element)
  (let ((last (list-queue-last list-queue)))
    (cond
     ((null? last)
      (let ((first (list element)))
	(list-queue-set-first! list-queue first)
	(list-queue-set-last! list-queue first)))
     (else
      (set-cdr! last (list element)) 
      (list-queue-set-last! list-queue (cdr last))))))

(define (list-queue-append! list-queue1 list-queue2)
  (let ((last (list-queue-last list-queue1))
	(first (list-queue-list list-queue2)))
    (cond
     ((null? last)
      list-queue2)
     ((null? first)
      list-queue1)
     (else
      (set-cdr! last first)
      (%make-list-queue (list-queue-list list-queue1)
			(list-queue-last list-queue2))))))

(define (list-queue-for-each proc list-queue)
  (for-each proc (list-queue-list list-queue)))

(define (list-queue-map! proc list-queue)
  (do ((first (list-queue-list list-queue) (cdr first)))
      ((null? first))
    (set-car! first (proc (car first)))))

(define (list-queue-map proc list-queue)
  (receive (first last)
      (let loop ((first (list-queue-list list-queue)))
	(if (null? first)
	    (values '() '())
	    (let*
		((element (car first))
		 (first (cdr first)))
	      (if (null? first)
		  (let ((last (list (proc element))))
		    (values last last))
		  (receive (first last)
		      (loop first)
		    (values (cons (proc element) first) last))))))
    (%make-list-queue first last)))
