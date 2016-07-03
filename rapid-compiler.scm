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

(import (scheme base)
	(scheme process-context)
	(scheme write)
	(rapid and-let)
	(rapid error)
	(rapid format)
	(rapid args-fold)
	(rapid version-etc)
	(rapid syntax)
	(rapid libraries)
	(rapid compiler))

(define (help)
  (write-string
   (format "Usage: ~a [OPTION] file\n  \
              -d, --debug   print more information about progress\n  \
              -I, --include prepends a library search directory\n  \
              -h, --help    display this help and exit\n  \
              -v, --version output version information and exit\n"
	   (car (command-line))))
  (newline)
  (emit-bug-reporting-address))

(let-values
    (((input)
      (args-fold (cdr (command-line))
		 (list
		  (option '(#\d "debug") #f #f
			  (lambda (option name arg input)
			    (current-log-level 'info)
			    input))
		  (option '(#\h "help") #f #f
			  (lambda (option name arg input)
			    (help)
			    (exit)))
		  (option '(#\v "version") #f #f
			  (lambda (option name arg input)
			    (version-etc "rapid-compiler")
			    (exit)))
		  (option '(#\I "include") #t #t
			  (lambda (option name arg input)
			    (current-library-directories
			     (cons arg (current-library-directories)))
			    input)))
		 (lambda (option name arg input)
		   (rapid-error 0 "invalid-option ‘~a’" name)
		   (help)
		   (exit 1))
		 (lambda (operand input)
		   operand)
		 #f)))
 
  (unless input
    (rapid-error 0 "no input file given")
    (help)
    (exit 1))

  (and-let*
      ((expanded-program (compile input)))
    (write-shared expanded-program)
    (newline)))
