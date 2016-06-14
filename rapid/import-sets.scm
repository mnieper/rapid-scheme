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

(define-record-type <export-spec>
  (make-export-spec source target)
  export-spec?
  (source export-spec-source)
  (target export-spec-target))

(define (make-exports)
  (imap identifier-comparator))

(define (exports-add exports source target)
  (let ((target-identifier (unwrap-syntax target)))
    (cond
     ((imap-ref/default exports target-identifier #f)
      => (lambda (export-spec)
	   (raise-syntax-error target
			       "identifier ‘~a’ is already bound"
			       (identifier->symbol target-identifier))
	   (raise-syntax-note (export-spec-target export-spec)
			      "previous binding was here")
	   #f))
     (else
      (imap-replace exports
		    target-identifier
		    (make-export-spec source target))))))

(define (exports-ref exports target)
  (imap-ref exports
	    (unwrap-syntax target)
	    (lambda ()
	      (raise-syntax-error target
				  "identifier ‘~a’ not bound"
				  (syntax->datum target))
	      #f)))

(define (exports-delete exports target)
  (receive (map ok)
      (imap-search
       exports
       (unwrap-syntax target)
       (lambda (insert ignore)
	 (raise-syntax-error target
			     "identifier ‘~a’ not bound"
			     (syntax->datum target))
	 (ignore #f))
       (lambda (key update remove)
	 (remove #t)))
    (and ok map)))

;;; Import sets

;; Imports is a procedure taking a set of identifiers (realized as an
;; imap) and produces a map mapping each identifier to the
;; syntax of the exported
;; identifier.

(define-record-type <import-set>
  (%make-import-set library-name-syntax imports)
  import-set?
  (library-name-syntax import-set-library-name-syntax)
  (imports import-set-imports))

(define (make-import-set syntax)
  (receive (library-name-syntax imports)
      (let loop ((syntax syntax))
	(let ((datum (unwrap-syntax syntax)))
	  (cond
	   ((list? datum)
	    (if (and (> (length datum) 1) (list? (unwrap-syntax (cadr datum))))
		(receive (library-name-syntax imports)
		    (loop (cadr datum))
		  (values
		   library-name-syntax
		   (case (syntax->datum (car datum))
		     ((only)
		      (imports-only imports (identifier-list (cddr datum))))
		     ((except)
		      (imports-except imports (identifier-list (cddr datum))))
		     ((prefix)
		      (cond
		       ((= (length datum) 3)
			(and (identifier-syntax? (list-ref datum 2))
			     (values library-name-syntax
				     (imports-prefix imports
						     (list-ref datum 2)))))
		       (else
			(raise-syntax-error syntax "bad prefix import set")
			#f)))
		     ((rename)
		      (imports-rename imports (rename-map (cddr datum))))
		     (else
		      (raise-syntax-error syntax "invalid import set")))))
		(and (library-name? syntax)
		     (values syntax (make-imports syntax)))))
	   (else
	    (raise-syntax-error syntax "bad import set")
	    #f))))
    (%make-import-set library-name-syntax imports)))

(define (import-set-modify import-set export-mapping)
  (import-set-imports import-set) export-mapping)

(define (make-imports syntax)
  (lambda (identifiers)
    (imap-map-values
     (lambda (identifier value)
       (let ((target (derive-syntax identifier syntax)))
	 (make-export-spec target target)))
     identifiers)))

;; TODO: Check the following procs

(define (imports-only imports syntax*)
  (lambda (identifiers)
    (let ((exports (imports identifiers)))
      (let loop ((only-exports (make-exports))
		 (syntax* syntax*))
	(if (null? syntax*)
	    only-exports
	    (let ((only-target (car syntax*)))
	      (cond
	       ((exports-ref exports only-target)
		=> (lambda (export-spec)
		     (loop (exports-add exports
					(export-spec-source export-spec)
					only-target)
			   (cdr syntax*))))
	       (else
		(loop (only-exports (cdr syntax*)))))))))))
	     
(define (imports-except imports syntax*)
  (lambda (identifiers)
    (let loop ((except-exports (imports identifiers))
	       (syntax* syntax*))
      (if (null? syntax*)
	  except-exports
	  (cond
	   ((exports-delete except-exports (car syntax*))
	    => (lambda (exports)
		 (loop exports (cdr syntax*))))
	   (else
	    (loop except-exports (cdr syntax*))))))))

(define (imports-prefix imports syntax)
  (let ((prefix (symbol->string (syntax->datum syntax))))
    (lambda (identifiers)
      (imap-fold
       (lambda (target-identifier export-spec prefix-exports)
	 (let ((prefix-identifier
		(symbol->identifier
		 (string->symbol
		  (string-append
		   prefix
		   (symbol->string (identifier->symbol target-identifier)))))))
	   (exports-add prefix-exports
			(export-spec-source export-spec)
			(derive-syntax prefix-identifier syntax))))
       (make-exports) (imports identifiers)))))

;; rename-map is an imap that maps identifiers to identifier syntax

(define (imports-rename imports rename-map)
  (lambda (identifiers)
    (imap-fold
     (lambda (target-identifier export-spec rename-exports)
       (cond
	((imap-ref/default rename-map target-identifier #f)
	 => (lambda (rename-target)
	      (exports-add rename-exports
			   (export-spec-source export-spec)
			   rename-target)))
	(else
	 (imap-replace rename-exports target-identifier export-spec))))
     (make-exports) (imports identifiers))))

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
