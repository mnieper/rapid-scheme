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

(define current-identity-counter (make-parameter 0))

(define-record-type <identifier>
  (make-identifier symbol closure identity hash)
  identifier?
  (symbol identifier->symbol)
  (closure identifier-closure)
  (identity identifier-identity)
  (hash identifier-hash))

(define make-synthetic-identifier
  (case-lambda
   ((symbol)
    (make-synthetic-identifier symbol '()))
   ((symbol closure)
    (let ((identity (current-identity-counter)))
      (current-identity-counter (+ identity 1))
      (make-identifier symbol closure identity identity)))))

(define (symbol->identifier symbol)
  (make-identifier symbol '() #f (symbol-hash symbol)))

(define (close-syntax identifier environment)
  (make-synthetic-identifier (identifier->symbol identifier)
			     (cons environment (identifier-closure identifier))))
  
(define (bound-identifier=? identifier1 identifier2)
  (or (and (not (alias? identifier1))
	   (not (alias? identifier2))
	   (eq? (identifier->symbol identifier1)
		(identifier->symbol identifier2)))
      (eq? identifier1 identifier2)))

(define (identifier-ordering identifier1 identifier2)
  (let ((hash1 (identifier-hash identifier1))
	(hash2 (identifier-hash identifier2)))
    (or (< hash1 hash2)
	(and (= hash1 hash2)
	     (not (alias? identifier1))
	     (not (alias? identifier2))
	     (string<? (symbol->string identifier1)
		       (symbol->string identifier2))))))

(define (alias? identifier)
  (and (identifier-identity identifier)
       #t))

(define identifier-comparator
  (make-comparator identifier? bound-identifier=? identifier-ordering
		   identifier-identity))
