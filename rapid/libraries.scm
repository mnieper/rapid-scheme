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

(define current-library-directories
  (make-parameter '("." "./lib")))

(define (read-library library-name-syntax)
  (and-let*
      ((library-definition-syntax
	(read-library-definition library-name-syntax)))
    ;; TODO: Do something with it
    library-definition-syntax))

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
    (and-let*
	(((or (not (null? directories))
	      (begin (raise-syntax-error library-name-syntax
					 "library definition of ‘~a’ not found"
					 library-name)
		     #f)))
	 (source (locate-library (car directories)))
	 ((file-exists? source))
	 (reader (read-file source #f library-name-syntax)))
      (let loop ()
	(let ((syntax (reader)))
	  (cond
	   ((eof-object? syntax)
	    (directory-loop (cdr directories)))
	   (else
	    ;; check whether header or loop ...
	    #f)))))))
