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

(define-record-type <iset>
  (make-iset comparator elements)
  iset?
  (comparator iset-comparator)
  (elements iset-elements iset-set-elements!))

(define (iset comparator . elements)
  (let loop ((elements elements) (set (make-iset comparator '())))
    (if (null? elements)
	set
	(loop (cdr elements) (iset-adjoin set (car elements))))))

(define (iset-empty? set)
  (null? (iset-elements set)))

(define (iset-member? set obj)
  (call-with-current-continuation
   (lambda (return)
     (let ((compare (iset-comparator set))
	   (elements (iset-elements set)))
       (let
	   ((tail 
	     (let loop ((elements elements))
	       (when (null? elements)
		 (return #f))
	       (if (compare obj (car elements))
		   (cdr elements)
		   (cons (car elements) (loop (cdr elements)))))))
	 (iset-set-elements! set (cons obj tail))
	 #t)))))

(define (iset-adjoin set obj)
  (if (iset-member? set obj)
      set
      (let ((compare (iset-comparator set))
	    (elements (iset-elements set)))
	(make-iset compare (cons obj elements)))))

(define (iset-delete set obj)
  (if (iset-member? set obj)
      (let ((compare (iset-comparator set))
	    (elements (iset-elements set)))
	(make-iset compare (cdr elements)))))

(define (iset->list set)
  (iset-elements set))

(define (iset-union set . sets)
  (let loop1 ((set set) (sets sets))
    (if (null? sets)
	set
	(let loop2 ((set set) (elements (iset-elements (car sets))))
	  (if (null? elements)
	      (loop1 set (cdr sets))
	      (loop2 (iset-adjoin set (car elements)) (cdr elements)))))))
