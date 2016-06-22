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

;;; Libraries

(define-record-type <library-definition>
  (%make-library exports import-sets body)
  library?
  (exports library-exports)
  (import-sets library-import-sets library-set-import-sets!)
  (body library-body library-set-body!))

(define (make-library)
  (%make-library (make-exports) (list-queue) (list-queue)))

(define (import-sets->library import-sets)
  (%make-library '() import-sets '()))

(define (add-export-spec! library syntax)
  (let ((exports (library-exports library))
	(export-spec (unwrap-syntax syntax)))
    (cond
     ((identifier? export-spec)
      (exports-add! exports syntax syntax))
     ((and-let*
	  (((tagged-list? export-spec 'rename 3))
	   ((= (length export-spec) 3))
	   (binding-syntax (list-ref export-spec 1))
	   (external-syntax (list-ref export-spec 2))
	   (identifier? (unwrap-syntax binding-syntax))
	   (identifier? (unwrap-syntax external-syntax)))
	(exports-add! exports
		      (list-ref export-spec 1)
		      (list-ref export-spec 2))))
     (else
      (raise-syntax-error syntax "bad export spec")))))

(define (add-import-set! library syntax)
  (list-queue-add-back! (library-import-sets library)
			(make-import-set syntax)))

(define (add-body-form! library syntax)
  (list-queue-add-back! (library-body library) syntax))

(define (library-declaration! library syntax)
  (let ((declaration (unwrap-syntax syntax)))
    (if (or (null? declaration) (not (list? declaration)))
	(raise-syntax-error syntax "bad library declaration")
	(case (syntax->datum (car declaration))
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
		    (clause (unwrap-syntax clause-syntax)))
	       (cond
		((or (null? clause) (not (list? clause)))
		 (raise-syntax-error clause-syntax "bad cond-expand clause"))
		((eq? (syntax->datum (car clause)) 'else)
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
  (let ((datum (unwrap-syntax syntax)))
    (cond
     ((identifier? datum)
      (memq datum (rapid-features)))
     ((and (not (null? datum)) (list? datum))
      (case (syntax->datum (car datum))
	((library)
	 (cond
	  ((= (length datum) 2)
	   (and (library-name? (cadr datum) raise-syntax-error)
		(or (equal? (syntax->datum (cadr datum))
			    '(rapid primitive))
		    (with-syntax-exception-guard
		     (lambda ()
		       (read-library-definition (cadr datum)))))))
	  (else
	   (raise-syntax-error syntax "bad library feature requirement")
	   #f)))
	((and)
	 (let loop ((syntax* (cdr datum)))
	   (if (null? syntax*)
	       #t
	       (let* ((r1 (feature? (car syntax*)))
		      (r2 (loop (cdr syntax*))))
		 (and r1 r2)))))
	((or)
	 (let loop ((syntax* (cdr datum)))
	   (if (null? syntax*)
	       #f
	       (let* ((r1 (feature? (car syntax*)))
		      (r2 (loop (cdr syntax*))))
		 (and r1 r2)))))
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
	      (cddr (unwrap-syntax library-definition-syntax)))
    (finalize-library! library)))

(define (read-program filename)
  (and-let*
      ((library (make-library))
       (reader (read-file filename #f #f)))
    (let loop ((import-declaration? #f))
      (let ((syntax (reader)))
	(cond
	 ((eof-object? syntax)
	  (raise-syntax-error #f "program contains no commands or definitions")
	  (finalize-library! library))
	 ((tagged-list? (unwrap-syntax syntax) 'import 1)
	  (for-each (lambda (syntax)
		      (add-import-set! library syntax))
		    (cdr (unwrap-syntax syntax)))
	  (loop #t))
	 (else
	  (unless import-declaration?
	    (raise-syntax-error #f "program contains no import declaration"))
	  (add-body-form! library syntax)
	  (generator-for-each (lambda (syntax)
				(add-body-form! library syntax))
			      reader)
	  (finalize-library! library)))))))

(define (finalize-library! library)    
  (library-set-body! library
		     (list-queue-list (library-body library)))
  (library-set-import-sets! library
			    (list-queue-list (library-import-sets library)))
  library)


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
			     ((datum (unwrap-syntax syntax))
			      ((or (tagged-list? datum 'define-library 2)
				   (begin (raise-syntax-warning
					   syntax
					   "invalid library definition")
				       (loop))))
			      ((library-name? (cadr datum) raise-syntax-warning))
			      ((equal? (syntax->datum (cadr datum)) library-name)))
			   syntax)
			 (loop))))))
	  (directory-loop (cdr directories)))))))

(define (tagged-list? datum tag n)
  (and (list? datum)
       (>= (length datum) n)
       (eq? (syntax->datum (car datum)) tag)))

(define (read-file* string-syntax* ci?)
  (apply gappend (map-in-order
		  (lambda (string-syntax)
		    (cond
		     ((locate-file string-syntax)
		      => (lambda (filename)
			   (read-file filename ci? string-syntax)))
		     (else
		      (generator))))
		  string-syntax*)))

(define (locate-file syntax)
  (let ((filename (unwrap-syntax syntax)))
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
