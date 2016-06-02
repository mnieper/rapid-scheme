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

(define-library (rapid records test)
  (export run-tests)
  (import (except (scheme base) define-record-type)
	  (rapid test)
	  (rapid records))
  (begin
    (define (run-tests)
      (test-begin "Records")

      (test-assert "Constructing a record yields an object of that type"
		   (let ()
		     (define-record-type <record>
		       (constructor)
		       pred?)
		      (pred? (constructor))))

      (test-equal "Constructors can initialize fields"
		  'a
		  (let ()
		    (define-record-type <record>
		      (constructor field1)
		      pred?
		      (field1 field1)
		      (field2 field2))
		    (field1 (constructor 'a))))

      (test-assert "Inheritance"
		   (let ()
		     (define-record-type <parent>
		       #f
		       parent?)
		     (define-record-type (<child> <parent>)
		       constructor
		       child?)
		     (parent? (constructor))))

      (test-equal "Shadowing of fields"
		  #(a b)
		  (let ()
		    (define-record-type <parent>
		      #f
		      parent?
		      (field parent-field parent-set-field!))
		    (define-record-type (<child> <parent>)
		      (constructor field)
		      child?
		      (field child-field))
		    (let ((record (constructor 'a)))
		      (parent-set-field! record 'b)
		      (vector
		       (child-field record)
		       (parent-field record)))))

      (test-end)

      #t)))
