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

;;; Export specs

(define-record-type <exports>
  (%make-export-mapping map)
  export-mapping?
  (map export-mapping-map export-mapping-set-map!))

(define (make-export-mapping)
  (%make-export-mapping (imap identifier-comparator)))

(define (export-mapping-add! export-mapping binding-syntax external-syntax)
  (let ((exports (export-mapping-map export-mapping))
	(external-identifier (unwrap-syntax external-syntax)))
    (cond
     ((imap-ref/default exports external-identifier #f)
      (raise-syntax-error external-syntax
			  "name ‘~a’ already exported" external-identifier)
      #f)
     (else
      (export-mapping-set-map! export-mapping
			       (imap-replace exports
					     external-identifier
					     binding-syntax))))))

(define (export-mapping-ref export-mapping external-syntax)
  (imap-ref (export-mapping-map export-mapping)
	    (unwrap-syntax external-syntax)
	    (lambda ()
	      (raise-syntax-error external-syntax
				  "identifier ‘~a’ not exported"
				  (syntax->datum external-syntax))
	      #f)))

(define (export-mapping-delete! export-mapping exported-identifier-syntax)
  (receive (map ok)
      (imap-search
       (export-mapping-map export-mapping)
       (unwrap-syntax exported-identifier-syntax)
       (lambda (insert ignore)
	 (raise-syntax-error exported-identifier-syntax
			     "identifier ‘~a’ not exported"
			     (syntax->datum exported-identifier-syntax))
	 (ignore #f))
       (lambda (key update remove)
	 (remove #t)))
    (export-mapping-set-map! export-mapping map)
    ok))

;;; Import sets

(define-record-type <import-set>
  (%make-import-set library-name-syntax modifier)
  import-set?
  (library-name-syntax import-set-library-name-syntax)
  (modifier import-set-modifier))

(define (make-import-set syntax)
  (receive (library-name-syntax modifier)
      (let loop ((syntax syntax))
	(let ((datum (unwrap-syntax syntax)))
	  (cond
	   ((list? datum)
	    (if (and (> (length datum) 1) (list? (unwrap-syntax (cadr datum))))
		(receive (library-name-syntax modifier)
		    (loop (cadr datum))
		  (values
		   library-name-syntax
		   (case (syntax->datum (car datum))
		     ((only)
		      (only-modifier modifier (identifier-list (cddr datum))))
		     ((except)
		      (except-modifier modifier (identifier-list (cddr datum))))
		     ((prefix)
		      (cond
		       ((= (length datum) 3)
			(and (identifier-syntax? (list-ref datum 2))
			     (values library-name-syntax
				     (prefix-modifier modifier
						      (list-ref datum 2)))))
		       (else
			(raise-syntax-error syntax "bad prefix import set")
			#f)))
		     ((rename)
		      (rename-modifier modifier (rename-map (cddr datum))))
		     (else
		      (raise-syntax-error syntax "invalid import set")))))
		(and (library-name? syntax)
		     (values syntax (lambda (exports) exports)))))
	   (else
	    (raise-syntax-error syntax "bad import set")
	    #f))))
    (%make-import-set library-name-syntax modifier)))

(define (import-set-modify import-set export-mapping)
  (import-set-modifier import-set) export-mapping)

(define (only-modifier modifier syntax*)
  (lambda (export-mapping)
    (let ((export-mapping (modifier export-mapping))
	  (only-export-mapping (make-export-mapping)))
      (let loop ((syntax* syntax*))
	(if (null? syntax*)
	    only-export-mapping
	    (let ((only-exported-identifier-syntax (car syntax*)))
	      (and-let*
		  ((bound-identifier-syntax
		    (export-mapping-ref export-mapping
					only-exported-identifier-syntax)))
		(export-mapping-add! only-export-mapping bound-identifier-syntax
				     only-exported-identifier-syntax))
	      (loop (cdr syntax*))))))))

(define (except-modifier modifier syntax*)
  (lambda (export-mapping)
    (let ((export-mapping (modifier export-mapping)))
      (do ((syntax* syntax* (cdr syntax*)))
	  ((null? syntax*) export-mapping)
	(export-mapping-delete! export-mapping (car syntax*))))))

(define (prefix-modifier modifier syntax)
  (let ((prefix (symbol->string (syntax->datum syntax))))
    (lambda (export-mapping)
      (%make-export-mapping
       (imap-map (lambda (exported-identifier-syntax)
		   (derive-syntax
		    (symbol->identifier
		     (string->symbol
		      (string-append
		       prefix
		       (symbol->string (syntax->datum exported-identifier-syntax)))))
		    syntax))
		 (export-mapping-map (modifier export-mapping)))))))

(define (rename-modifier modifier rename-map)
  (lambda (export-mapping)
    (let ((rename-export-mapping (make-export-mapping)))
      (imap-for-each
       (lambda (key value)
	 (cond
	  ((imap-ref/default rename-map key #f)
	   => (lambda (renamed-identifier-syntax)
		(export-mapping-add! rename-export-mapping
				     value
				     renamed-identifier-syntax)))
	  (else
	   (export-mapping-add! rename-export-mapping value key))))
       (export-mapping-map (modifier export-mapping)))
      rename-export-mapping)))

(define library-name?
  (case-lambda
   ((syntax)
    (library-name? syntax raise-syntax-error))
   ((syntax raise)
    (let ((datum (unwrap-syntax syntax)))
      (or (and (list? datum)
	       (let loop ((datum datum))
		 (or (null? datum)
		     (let ((element (syntax->datum (car datum))))
		       (and (or (and (exact-integer? element) (>= element 0))
				(symbol? element))
			    (loop (cdr datum)))))))
	  (begin (raise syntax "bad library name")
		 #f))))))

(define (identifier-syntax? syntax)
  (or (identifier? (unwrap-syntax syntax))
      (begin
	(raise-syntax-error syntax "bad identifier")
	#f)))

(define (identifier-list syntax*)
  (let loop ((syntax* syntax*))
    (cond
     ((null? syntax*)
      '())
     ((identifier-syntax? (car syntax*))
      (cons (car syntax*) (loop (cdr syntax*))))
     (else
      (loop (cdr syntax*))))))

(define (rename-map syntax*)
  (fold-right
   (lambda (rename-syntax rename-map)
     (if (rename? rename-syntax)
	 (let ((identifier-syntax (car (unwrap-syntax rename-syntax))))
	   (cond
	    ((imap-ref/default rename-map (unwrap-syntax identifier-syntax) #f)
	     (raise-syntax-error identifier-syntax
				 "identifier ‘~a’ is already being renamed"
				 identifier-syntax)
	     rename-map)
	    (else
	     (imap-replace rename-map
			   (unwrap-syntax identifier-syntax)
			   (cdr (unwrap-syntax rename-syntax))))))
	 rename-map))
   (imap identifier-comparator) syntax*))

(define (rename? syntax)
  (let ((datum (unwrap-syntax syntax)))
    (cond
     ((and (list? datum) (= (length datum) 2))
      (and (identifier-syntax? (car datum))
	   (identifier-syntax? (cadr datum))))
     (else
      (raise-syntax-error syntax "bad rename")
      #f))))
