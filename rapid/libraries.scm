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

(define-record-type <library-definition>
  (%make-library exports imports body)
  library?
  (exports library-exports library-set-exports!)
  (imports library-environment)
  (body library-body))

(define symbol-comparator (make-eq-comparator))

(define (make-library)
  (%make-library (imap symbol-comparator) (make-environment) (list-queue)))
		      
(define (add-export-spec! library syntax)
  (define (add! binding-syntax external-syntax)
    (let
	((exports (library-exports library))
	 (external-symbol (syntax-datum external-syntax)))
      (if (imap-ref/default exports external-symbol #f)
	  (raise-syntax-error external-syntax
			      "name ‘~a’ already exported" external-symbol)
	  (library-set-exports! library
				(imap-replace exports
					      external-symbol
					      (syntax-datum binding-syntax))))))
  (let ((export-spec (syntax-datum syntax)))
    (cond
     ((symbol? export-spec)
      (add! syntax syntax))
     ((and-let*
	  (((tagged-list? export-spec 'rename 3))
	   ((= (length export-spec) 3))
	   (binding-syntax (list-ref export-spec 1))
	   (external-syntax (list-ref export-spec 2))
	   (symbol? (syntax-datum binding-syntax))
	   (symbol? (syntax-datum external-syntax)))
	(add! (list-ref export-spec 1) (list-ref export-spec 2))))
     (else
      (raise-syntax-error syntax "bad export spec")))))
	
(define (add-import-set! library syntax)
  (environment-add-import-set! (library-environment library) syntax))

(define (add-body-form! library syntax)
  (list-queue-add-back! (library-body library) syntax))

(define (library-declaration! library syntax)
  (let ((declaration (syntax-datum syntax)))
    (if (or (null? declaration) (not (list? declaration)))
	(raise-syntax-error syntax "bad library declaration")
	(case (syntax-datum (car declaration))
	  ((export)
	   (for-each (lambda (syntax)
		       (add-export-spec! library syntax))
		     (cdr declaration)))
	  ((import)
	   (for-each (lambda (syntax)
		       (add-import-set! library syntax))
		     (cdr declaration)))
	  ((begin)
	   (for-each (lambda (syntax)
		       (add-body-form! library syntax))
		     (cdr declaration)))
	  ((include)
	   (generator-for-each (lambda (syntax)
				 (add-body-form! library syntax))
			       (read-file* (cdr declaration) #f)))
	  ((include-ci)
	   (generator-for-each (lambda (syntax)
				 (add-body-form! library syntax))
			       (read-file* (cdr declaration) #t)))
	  ((include-library-declarations)
	   (generator-for-each (lambda (syntax)
				 (library-declaration! library syntax))
			       (read-file* (cdr declaration) #f)))
	  ((cond-expand)
	   (do ((clause-syntax* (cdr declaration) (cdr clause-syntax*)))
	       ((null? clause-syntax*))
	     (let* ((clause-syntax (car clause-syntax*))
		    (clause (syntax-datum clause-syntax)))
	       (cond
		((or (null? clause) (not (list? clause)))
		 (raise-syntax-error clause-syntax "bad cond-expand clause"))
		((eq? (syntax-datum (car clause)) 'else)
		 (if (null? (cdr clause-syntax*))
		     (for-each (lambda (syntax)
				 (library-declaration! library syntax))
			       (cdr clause))
		     (raise-syntax-error clause-syntax "else clause not last")))
		((feature? (car clause))
		 (for-each (lambda (syntax)
			     (library-declaration! library syntax))
			   (cdr clause)))))))
	  (else
	   (raise-syntax-error (car declaration)
			       "invalid library declaration"))))))

(define (feature? syntax)
  (let ((datum (syntax-datum syntax)))
    (cond
     ((symbol? datum)
      (memq datum (rapid-features)))
     ((and (not (null? datum)) (list? datum))
      (case (syntax-datum (car datum))
	;; TODO: library
	;; TODO: and
	;; TODO: or	
	((not)
	 (cond
	  ((= (length datum) 2)
	   (not (feature? (cadr datum))))
	  (else
	   (raise-syntax-error syntax "bad not feature requirement"))))
	(else
	 (raise-syntax-error syntax "invalid feature requirement")
	 #f)))
     (else
      (raise-syntax-error syntax "bad feature requirement")
      #f))))

(define current-library-directories
  (make-parameter '("." "./lib")))

(define (read-library library-name-syntax)
  (and-let*
      ((library-definition-syntax
	(read-library-definition library-name-syntax))
       (library (make-library)))
    (for-each (lambda (syntax)
		(library-declaration! library syntax))
	      (cddr (syntax-datum library-definition-syntax)))))

(define (read-library-definition library-name-syntax)

  (define library-name (syntax->datum library-name-syntax))

  (define (locate-library directory)
    (let loop ((filename directory) (library-name library-name))
      (if (null? library-name)
	  (string-append filename ".sld")
	  (let ((element (car library-name)))
	    (loop (path-join filename ((if (symbol? element)
					   symbol->string
					   number->string) element))
		  (cdr library-name))))))
  
  (let directory-loop ((directories (current-library-directories)))
    (cond
     ((null? directories)
      (raise-syntax-error library-name-syntax
			  "library definition of ‘~a’ not found"
			  library-name)
      #f)
     (else
      (or (and-let*
	      ((source (locate-library (car directories)))
	       ((file-exists? source))
	       (reader (read-file source #f library-name-syntax)))
	    (let loop ()
	      (let ((syntax (reader)))
		(and (not (eof-object? syntax))
		     (or (and-let*
			     ((datum (syntax-datum syntax))
			      ((or (tagged-list? datum 'define-library 2)
				   (begin (raise-syntax-error
					   syntax
					   "invalid library definition")
				       (loop))))
			      ((library-name? (cadr datum)))
			      ((equal? (syntax->datum (cadr datum)) library-name)))
			   syntax)
			 (loop))))))
	  (directory-loop (cdr directories)))))))

(define (tagged-list? datum tag n)
  (and (list? datum)
       (>= (length datum) n)
       (eq? (syntax-datum (car datum)) tag)))

(define (library-name? syntax)
  (let ((datum (syntax-datum syntax)))
    (or (and (list? datum)
	     (let loop ((datum datum))
	       (or (null? datum)
		   (let ((element (syntax-datum (car datum))))
		     (and (or (and (exact-integer? element) (>= element 0))
			      (symbol? element))
			  (loop (cdr datum)))))))
	(begin (raise-syntax-error syntax "bad library name")
	       #f))))

(define (read-file* string-syntax* ci?)
  (apply gappend (map-in-order
		  (lambda (string-syntax)
		    (cond
		     ((locate-file string-syntax)
		      => (lambda (filename)
			   (read-file filename string-syntax ci?)))
		     (else
		      (generator))))
		  string-syntax*)))

(define (locate-file syntax)
  (let ((filename (syntax-datum syntax)))
    (cond
     ((string? filename)
      (cond
       ((syntax-source-location syntax)
	=> (lambda (location)
	     (path-join (path-directory (source-location-source location))
			filename)))
       (else
	filename)))
     (else
      (raise-syntax-error syntax "bad string literal")
      #f))))
