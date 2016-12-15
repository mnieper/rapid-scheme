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

(define-library (rapid map)
  (export make-map
	  map? map-contains? map-empty? map-disjoint?
	  map-ref map-ref/default map-key-comparator
	  map-set map-set!
	  map-delete map-delete!
	  map-intern map-intern!
	  map-update map-update!
	  map-replace map-replace!
      	  map-size map-find map-count map-keys map-values map-entries
	  map-map map-for-each map-fold map-filter map-filter! map-remove map-remove!
	  map-map->list
	  map-partition map-partition!
	  map-copy map-empty-copy map->alist alist->map
	  map-union map-intersection map-difference map-xor
	  map-union! map-intersection! map-difference! map-xor!)
  (import (scheme base)
	  (scheme case-lambda)
	  (rapid receive)
	  (rapid comparator)
	  (rapid set))
  (include "map.scm"))
