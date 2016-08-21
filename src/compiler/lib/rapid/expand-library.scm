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

(define (expand-library library)

  (define-syntax with-import-sets
    (syntax-rules ... ()
      ((with-import-sets import-sets body1 body2 ...)
       (with-syntactic-environment (make-syntactic-environment)
	 (for-each import-import-set! import-sets)
	 body1 body2 ...))))
  
  (define definitions (list-queue))

  (define (add-runtime-definitions! list-queue)
    (set! definitions
	  (list-queue-append! list-queue definitions)))
  
  (define (add-definitions! list-queue)
    (set! definitions
	  (list-queue-append! definitions list-queue)))
  
  (define syntactic-environments
    (imap library-name-comparator
	  (list (symbol->identifier 'rapid)
		(symbol->identifier 'primitive))
	  primitive-environment))

  (define (syntactic-environment-set! library-name syntactic-environment)
    (set! syntactic-environments
	  (imap-replace syntactic-environments
			library-name
			syntactic-environment)))
  
  (define (syntactic-environment-intern! library-name-syntax runtime?)
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
			      library-name-syntax runtime?)))
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
	syntactic-environment))))

  (define (import-import-set! import-set)
    (and-let*
	((syntactic-environment
	  (syntactic-environment-intern! (import-set-library-name-syntax
					  import-set)
					 #f)))
      (import-syntactic-environment! syntactic-environment
				     (import-set-imports import-set))
      #t))
  
  (define (load-syntactic-environment! library-name-syntax runtime?)
    (let ((library-name (map unwrap-syntax
			     (unwrap-syntax library-name-syntax))))
      (and-let* ((library (read-library-definition library-name-syntax)))
	(with-import-sets (library-definition-import-sets library)
	  ((if runtime? add-runtime-definitions! add-definitions!)
	   (expand-top-level! (library-definition-body library)))
	  (let ((environment (current-syntactic-environment)))	
	    (with-syntactic-environment (make-syntactic-environment)
	      (export-syntactic-environment! environment
					     (library-definition-exports library))
	      (current-syntactic-environment)))))))

  (with-import-sets (library-definition-import-sets library)
    (add-definitions!
     (expand-top-level! (library-definition-body library)))
    (let*
	((expression
	  (make-letrec*-expression (list-queue-list definitions)
				   ;; TODO: Maybe intern the syntactic environments
				   (list (make-undefined #f))
				   #f))
	 (expression
	  (fix-letrec expression))
	 (expression
	  (cps-transform expression))
	 (expression
	  (mutable-variable-eliminate expression))
	 (expression
	  (introduce-let expression))
	 (expression
	  (bind-procedures expression))
	 (expression
	  (lambda-lift expression))
	 (expression
	  (closure-convert expression)))
      (values
       ;; FIXME: Return the locations of the exported variables
       #f
       expression))))

;; Local Variables:
;; eval: (put 'with-import-sets 'scheme-indent-function 1)
;; End:
