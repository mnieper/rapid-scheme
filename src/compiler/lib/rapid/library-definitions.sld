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

(define-library (rapid library-definitions)
  (export read-library-definition
	  read-program
	  read-file*
	  expand-cond-expand
	  import-sets->library-definition
	  library-definition?
	  library-definition-exports
	  library-definition-import-sets
	  library-definition-body
	  current-library-directories)
  (import (scheme file)
	  (rapid base)
	  (rapid and-let)
	  (rapid lists)
	  (rapid list-queues)
	  (rapid comparators)
	  (rapid generators)
	  (rapid immutable-maps)
	  (rapid features)
	  (rapid identifiers)
	  (rapid syntax)
	  (rapid import-sets)
	  (rapid read)
	  (rapid paths))
  (include "library-definitions.scm"))
