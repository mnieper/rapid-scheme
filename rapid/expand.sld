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

(define-library (rapid expand)
  (export expand-top-level!
	  expand-body
	  expand-expression
	  expand-transformer
	  expand-into-syntax-definition
	  expand-into-definition
	  expand-into-sequence
	  expand-into-expression
	  expand-into-transformer
	  expand-syntax!)
  (import (scheme base)
	  (rapid and-let)
	  (rapid lists)
	  (rapid list-queues)
	  (rapid identifiers)
	  (rapid syntax)
	  (rapid syntactic-environments)
	  (rapid expressions))
  (include "expand.scm"))
    
