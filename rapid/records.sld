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

(define-library (rapid records)
  (export define-record-type)
  (cond-expand
   ((library (srfi 131))
    (import (srfi 131)))
   ((library (srfi 99))
    (import (srfi 99)))
   (else
    (import (rename (scheme base) (define-record-type scheme-define-record-type))
	    (scheme case-lambda)
	    (rapid vectors)
	    (rapid comparators)
	    (rapid immutable-sets)
	    (rapid immutable-maps))
    (include "records.scm"))))
