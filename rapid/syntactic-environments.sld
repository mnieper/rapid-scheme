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

(define-library (rapid syntactic-environments)
  (export make-location location? location-syntax
	  make-transformer transformer? transformer-proc transformer-syntax
	  make-primitive primitive? primitive-value primitive-syntax
	  make-primitive-transformer primitive-transformer?
	  primitive-transformer-name
	  with-syntactic-environment
	  with-scope
	  define-syntactic-environment
	  define-transformer
	  define-auxiliary-syntax
	  make-syntactic-environment
	  syntactic-environment?
	  current-syntactic-environment
	  import-syntactic-environment!
	  export-syntactic-environment!
	  insert-syntactic-binding!
	  lookup-denotation!
	  identifier=?
	  free-identifier=?
	  make-free-identifier-comparator
	  maybe-isolate)
  (import (scheme case-lambda)
	  (rapid base)
	  (rapid and-let)
	  (rapid comparators)
	  (rapid immutable-maps)
	  (rapid identifiers)
	  (rapid syntax)
	  (rapid import-sets)
	  (rapid libraries))
  (include "syntactic-environments.scm"))
	
;; Local Variables:
;; eval: (put 'with-syntactic-environment 'scheme-indent-function 1)
;; eval: (put 'with-scope 'scheme-indent-function 0)
;; eval: (put 'maybe-isolate 'scheme-indent-function 1)
;; eval: (put 'define-syntactic-environment 'scheme-indent-function 'defun)
;; eval: (put 'define-transformer 'scheme-indent-function 'defun)
;; eval: (put 'define-auxiliary-syntax 'scheme-indent-function 'defun)
;; eval: (font-lock-add-keywords 'scheme-mode
;;                               '(("(\\(define-syntactic-environment\\)\\>"
;;                                  1 font-lock-keyword-face)
;;                                 ("(\\(define-auxiliary-syntax\\)\\>"
;;                                  1 font-lock-keyword-face)
;;                                 ("(\\(define-transformer\\)\\>"
;;                                  1 font-lock-keyword-face)))
;; End:
