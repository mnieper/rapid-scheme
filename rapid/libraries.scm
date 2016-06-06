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
  (make-library exports imports body)
  library?
  (exports library-exports)
  ;; exports is an imap mapping exported names to library internal
  ;; imports is WHAT? (something like a function; needs to load libs)
  ;; body is a list
  
  (imports library-imports)
  (body library-body))

(define symbol-comparator (make-eq-comparator))

(define current-library-directories
  (make-parameter '("." "./lib")))

(define (read-library library-name-syntax)
  (and-let*
      ((library-definition-syntax
	(read-library-definition library-name-syntax)))
    (do ((declarations (cddr (syntax-datum library-definition-syntax))
		       (cdr declarations))
	 (exports (imap symbol-comparator))
	 (imports #f)
	 (body (list-queue)))
	((null? declarations)
	 (make-library exports imports body))
      )))

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

