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

;;; The table of read libraries

(define identifier<? (comparator-ordering-predicate identifier-comparator))
(define identifier-hash (comparator-hash-function identifier-comparator))

(define (library-element-type-test obj)
   (lambda (obj)
     (or (identifier? obj)
	 (and (exact-integer? obj) (>= obj 0)))))

(define (library-element=? element1 element2)
  (if (identifier? element1)
      (and (identifier? element2)
	   (bound-identifier=? element1 element2))
      (and (exact-integer? element2)
	   (= element1 element2))))

(define (library-element<? element1 element2)
  (if (identifier? element1)
      (or (exact-integer? element2)
	  (identifier<? element1 element2))
      (and (exact-integer? element2)
	   (< element1 element2))))

(define (library-element-hash element)
  (if (identifier? element)
      (identifier-hash element)
      (number-hash element)))

(define library-element-comparator
  (make-comparator library-element-type-test
		   library-element=?
		   library-element<?
		   library-element-hash))

(define library-name-comparator
  (make-list-comparator library-element-comparator list? null? car cdr))

(define-record-type <environment>
  (%make-environment)
  environment?)

(define (make-environment import-sets body)

  (define syntactic-environment-table
    (imap library-name-comparator
	  (list (symbol->identifier 'rapid)
		(symbol->identifier 'primitive))
	  #t))
  
  (define (syntactic-environment-intern! library-name-syntax)
    (call-with-current-continuation
     (lambda (return)
       (let ((library-name
	      (map unwrap-syntax (unwrap-syntax library-name-syntax))))
	 (cond
	  ((imap-ref syntactic-environment-table
		     library-name
		     (lambda ()
		       (return (load-syntactic-environment! library-name-syntax)))))
	  (else
	   (raise-syntax-error library-name-syntax
			       "library references itself while loading")
	   #f))))))
  
  (define (load-syntactic-environment! library-name-syntax)
    (let ((library-name (map unwrap-syntax
			     (unwrap-syntax library-name-syntax))))
      (set! syntactic-environment-table
	    (imap-replace syntactic-environment-table library-name #f))
      (and-let* ((library (read-library library-name-syntax)))
	(import-syntactic-environment! (library-import-sets library))
	;; FIXME: Do something with the imported environment
	)))

  (define (import! import-set)
    (and-let*
	((syntactic-environment
	  (syntactic-environment-intern!
	   (import-set-library-name-syntax import-set))))
      ;; FIXME: Do something
      #t))
  
  (define (import-syntactic-environment! import-sets)
    (for-each import! import-sets))
  
  (define environment (%make-environment))

  (import-syntactic-environment! import-sets)
  ;; FIXME: Do something with the imported environment
  
  environment)
