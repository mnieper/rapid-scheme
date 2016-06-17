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

(define-record-type <exports>
  (%make-exports map specs)
  exports?
  (map exports-map exports-set-map!)
  (specs exports-specs))

(define (make-exports)
  (%make-exports (imap identifier-comparator) (list-queue)))

(define (exports-add! exports source target)
  (let ((target-identifier (unwrap-syntax target))
	(map (exports-map exports)))
    (cond
     ((imap-ref/default map target-identifier #f)
      => (lambda (export-spec)
	   (raise-syntax-error target
			       "identifier ‘~a’ is already bound"
			       (identifier->symbol target-identifier))
	   (raise-syntax-note (export-spec-target export-spec)
			      "previous binding was here")))
     (else
      (let ((export-spec (make-export-spec source target)))
	(exports-set-map! exports
			  (imap-replace map
					target-identifier
					export-spec))
	(list-queue-add-back! (exports-specs exports) export-spec))))))

(define (exports-ref exports target)
  (imap-ref (exports-map exports)
	    (unwrap-syntax target)
	    (lambda ()
	      (raise-syntax-error target
				  "identifier ‘~a’ not bound"
				  (syntax->datum target))
	      #f)))

(define (exports-for-each proc exports)
  (list-queue-for-each
   (lambda (export-spec)
     (proc (unwrap-syntax (export-spec-target export-spec)) export-spec))
   (exports-specs exports)))

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
		      (imports-except imports
				      (cddr datum)
				      (except-map (cddr datum))))
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
		      (imports-rename imports
				      (cddr datum)
				      (rename-map (cddr datum))))
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
    (let ((exports (make-exports)))
      (imap-for-each
       (lambda (identifier value)
	 (let ((target (derive-syntax identifier syntax)))
	   (exports-add! exports target target)))
       identifiers)
      exports)))

(define (imports-only imports syntax*)
  (lambda (identifiers)
    (let ((exports (imports identifiers))
	  (only-exports (make-exports)))
      (do ((syntax* syntax* (cdr syntax*)))
	  ((null? syntax*)
	   only-exports)
	(let ((only-target (car syntax*)))
	  (and-let*
	      ((export-spec (exports-ref exports only-target)))
	    (exports-add! only-exports
			  (export-spec-source export-spec)
			  only-target))))
      only-exports)))

(define (imports-except imports syntax* except-map)

  (lambda (identifiers)
    (let ((exports (imports identifiers))
	  (except-exports (make-exports)))

      (for-each
       (lambda (identifier-syntax)
	 ;; The following result is ignored, but an error may be raised.
	 (exports-ref exports identifier-syntax))
       syntax*)

      (exports-for-each
       (lambda (target-identifier export-spec)
	 (unless (imap-ref/default except-map
				   target-identifier
				   #f)
	   (exports-add! except-exports
			 (export-spec-source export-spec)
			 (export-spec-target export-spec))))
       exports)

      except-exports)))

(define (imports-prefix imports syntax)
  (let ((prefix (symbol->string (syntax->datum syntax))))
    (lambda (identifiers)
      (let ((prefix-exports (make-exports)))
	(exports-for-each
	 (lambda (target-identifier export-spec)
	   (let ((prefix-identifier
		  (symbol->identifier
		   (string->symbol
		    (string-append
		     prefix
		     (symbol->string (identifier->symbol target-identifier)))))))
	     (exports-add! prefix-exports
			   (export-spec-source export-spec)
			   (derive-syntax prefix-identifier syntax))))
	 (imports identifiers))
	prefix-exports))))

(define (imports-rename imports syntax* rename-map)
  (lambda (identifiers)
    (let ((exports (imports identifiers))
	  (rename-exports (make-exports)))
      (exports-for-each
       (lambda (target-identifier export-spec)
	 (unless (imap-ref/default rename-map target-identifier #f)
	   (exports-add! rename-exports
			 (export-spec-source export-spec)
			 (export-spec-target export-spec))))
       exports)
      (for-each
       (lambda (rename-syntax)
	 (and-let* ((rename-identifier-syntax
		     (car (unwrap-syntax rename-syntax)))     
		    (export-spec (exports-ref exports rename-identifier-syntax)))
	   (exports-add! rename-exports
			 (export-spec-source export-spec)
			 (imap-ref rename-map
				   (unwrap-syntax rename-identifier-syntax)))))
       syntax*)
      rename-exports)))

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

(define (except-map syntax*)
  (fold-right
   (lambda (identifier-syntax except-map)
     (if (identifier-syntax? identifier-syntax)
	 (cond
	  ((imap-ref/default except-map (unwrap-syntax identifier-syntax) #f)
	   (raise-syntax-error identifier-syntax
			       "identifier ‘~a’ is already being excepted"
			       (syntax->datum identifier-syntax))
	   except-map)
	  (else
	   (imap-replace except-map
			 (unwrap-syntax identifier-syntax)
			 identifier-syntax)))
	 except-map))
   (imap identifier-comparator) syntax*))
  
(define (rename-map syntax*)
  (fold-right
   (lambda (rename-syntax rename-map)
     (if (rename? rename-syntax)
	 (let ((identifier-syntax (car (unwrap-syntax rename-syntax))))
	   (cond
	    ((imap-ref/default rename-map (unwrap-syntax identifier-syntax) #f)
	     (raise-syntax-error identifier-syntax
				 "identifier ‘~a’ is already being renamed"
				 (syntax->datum identifier-syntax))
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
