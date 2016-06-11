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

(define-library (rapid syntax)
  (export make-syntax
	  syntax?
          unwrap-syntax syntax-set-datum!
	  syntax-source-location
	  syntax-context
	  syntax-reference syntax-set-reference!
	  syntax->datum
	  derive-syntax
	  make-source-location
	  source-location?
	  source-location-source
	  source-location-start-line
	  source-location-end-line
	  source-location-start-column
	  source-location-end-column
	  raise-syntax-note
	  raise-syntax-warning
	  raise-syntax-error
	  raise-syntax-fatal-error
	  with-syntax-exception-handler
	  with-syntax-exception-guard)
  (import (scheme case-lambda)
	  (scheme process-context)
	  (scheme write)
	  (rapid base)
	  (rapid format)
	  (rapid identifiers))
  (include "syntax.scm"))
