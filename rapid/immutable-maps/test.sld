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

(define-library (rapid immutable-maps test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid comparators)
	  (rapid immutable-maps))
  (begin
    (define (run-tests)
      (define comparator (make-comparator symbol? symbol=?
					  (lambda (x y)
					    (string<? (symbol->string x)
						      (symbol->string y)))
					  #f))

      (define integers (make-comparator integer? = < #f))
      (define (make-random-list length n)
	(let loop ((length length) (k 1))
	  (if (= length 0)
	      '()
	      (cons k (loop (- length 1) (modulo (* k 89) n))))))

      (test-begin "Immutable maps")

      (test-assert "Constructing an immutable maps yields an immutable map"
		   (imap? (imap comparator)))

      (test-equal "Replacing associations in a map"
		  2
		  (imap-ref (imap-replace (imap comparator 'a 1)
					  'a
					  2)
			    'a))

      (test-error "Trying to reference non-associated keys raises an error"
		  (imap-ref (imap comparator 'a 1) 'b))
      

      (test-equal "Default values"
		  2
		  (imap-ref/default (imap comparator 'a 1) 'b 2))
      

 
      (test-assert "Immutable maps contain their added elements"
		   (imap-contains? (imap-replace (imap integers) 42 'x)
				   42))

      (test-assert "Immutable maps only contain their added elements"
		   (not (imap-contains? (imap-replace (imap integers) 42 'x)
					43)))

      (test-assert "Inserting random elements in list"
		   (let*
		       ((random-list
			 (make-random-list 100 100000))
			(map
			 (let loop ((map (imap integers))
				    (random-list random-list))
			   (if (null? random-list)
			       map
			       (loop (imap-replace map (car random-list) #f)
				     (cdr random-list))))))
		     (let loop ((random-list random-list))
		       (or (null? random-list)
			   (and (imap-contains? map (car random-list))
				(loop (cdr random-list)))))))
      
      (test-assert "Deleting random elements in list"
		   (let*
		       ((random-list
			 (make-random-list 100 100000))
			(map
			 (let loop ((map (imap integers))
				    (random-list random-list))
			   (if (null? random-list)
			       map
			       (loop (imap-replace map (car random-list) #f)
				     (cdr random-list))))))
		     (let loop ((random-list random-list) (map map))
		       (or (null? random-list)
			   (let ((map (imap-delete map (car random-list))))
			     (and (not (imap-contains? map (car random-list)))
				  (loop (cdr random-list) map)))))))

      ;; TODO: CHECK BALANCE OF TREES

      (test-end)
      #t)))
