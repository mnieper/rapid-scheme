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

(define-library (rapid map test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid comparator)
	  (rapid map))
  (begin
    (define default-comparator (make-default-comparator))

    (define map (make-map default-comparator 1 'a 2 'b 3 'c 4 'd))
    
    (define (run-tests)
      (test-begin "maps")

      (test-assert "map?"
	(map? (make-map default-comparator)))

      (test-assert "map-empty?: empty"
	(map-empty? (make-map default-comparator)))

      (test-assert "map-empty?: not empty"
	(not (map-empty? (make-map default-comparator 1 'a 2 'b))))

      (test-assert "map-contains?: contains"
	(map-contains? (make-map default-comparator 1 'a 2 'b) 1))
      
      (test-assert "map-contains?: not contains"
	(not (map-contains? (make-map default-comparator 1 'a 2 'b) 3)))
      
      (test-assert "map-disjoint?: disjoint"
	(map-disjoint? (make-map default-comparator 1 'a 2 'b)
		       (make-map default-comparator 3 'c 4 'd)))

      (test-assert "map-disjoint?: not disjoint"
	(not (map-disjoint? (make-map default-comparator 1 'a 2 'b)
			    (make-map default-comparator 2 'b 3 'c))))
      
      (test-equal "map-ref: key found"
	'a
	(map-ref map 1))

      (test-error "map-ref: key not found"
        (map-ref map 42))
      
      (test-equal "map-ref/default"
	'answer
	(map-ref/default map 42 'answer))

      (test-equal "map-update: found"
	'(b)
	(map-ref (map-update map 2 list)
		 2))    
      
      (test-end))))
