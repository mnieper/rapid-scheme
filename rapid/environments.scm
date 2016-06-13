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
  (%make-environment )
  environment?)

;; XXX: Should return a syntactic environment
;; and some code
;; Check: in which form
;; And: Rename make-environment into which???
;; e.g. ---> expand-library?

(define (expand-library library)
  ;; XXX: should add (values <location>...) at the end of the body
  ;; and update the export map
  
  (define store
    ;; Locations for values for variables
    (imap identifier-comparator))
  
  (define syntactic-environments
    (imap library-name-comparator
	  (list (symbol->identifier 'rapid)
		(symbol->identifier 'primitive))
	  'primitive-library))

  (define (syntactic-environment-set! library-name syntactic-environment)
    (set! syntactic-environments
	  (imap-replace syntactic-environments
			library-name
			syntactic-environment)))
  
  (define (syntactic-environment-intern! library-name-syntax)
    (call-with-current-continuation
     (lambda (return)
       (let*
	   ((library-name
	     (map unwrap-syntax (unwrap-syntax library-name-syntax)))
	    (syntactic-environment
	     (imap-ref syntactic-environments
		       library-name
		       (lambda ()
			 (syntactic-environment-set! library-name #t)
			 (let ((syntactic-environment
				(load-syntactic-environment!
				 library-name-syntax)))
			   (syntactic-environment-set! library-name
							     syntactic-environment)
			    syntactic-environment)))))
	 (cond
	  ((eq? syntactic-environment #t)
	   (raise-syntax-error library-name-syntax
			       "library ‘~a’ references itself while loading"
			       (syntax->datum library-name-syntax))
	   (syntactic-environment-set! library-name #f)
	   #f)
	  (else
	   syntactic-environment))))))
  
  (define (load-syntactic-environment! library-name-syntax)
    (let ((library-name (map unwrap-syntax
			     (unwrap-syntax library-name-syntax))))
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

  (import-syntactic-environment! (library-import-sets library))
  ;; FIXME: Do something with the imported environment
  
  environment)
