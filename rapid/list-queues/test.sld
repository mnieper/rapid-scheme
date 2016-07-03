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

(define-library (rapid list-queues test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid list-queues))
  (begin
    (define (run-tests)
      (test-assert "list-queue?"
		   (list-queue? (list-queue)))

      (test-equal "list-queue-empty?"
		  '(#t #f)
		  (list (list-queue-empty? (list-queue))
			(let ((lq (list-queue)))
			  (list-queue-add-back! lq 1)
			  (list-queue-empty? lq))))
      
      (test-equal "list-queue-add-back!"
		  '(1 2 3)
		  (let ((lq (list-queue)))
		    (list-queue-add-back! lq 1)
		    (list-queue-add-back! lq 2)
		    (list-queue-add-back! lq 3)		    
		    (list-queue-list lq)))

      (test-equal "list-queue-for-each"
		  '(3 2 1)
		  (let ((lq (list-queue)))
		    (list-queue-add-back! lq 1)
		    (list-queue-add-back! lq 2)
		    (list-queue-add-back! lq 3)		    
		    (let ((lst '()))
		      (list-queue-for-each
		       (lambda (i)
			 (set! lst (cons i lst)))
		       lq)
		      lst)))

      (test-equal "list-queue-map!"
		  '(2 4 6)
		  (let ((lq (list-queue)))
		    (list-queue-add-back! lq 1)
		    (list-queue-add-back! lq 2)
		    (list-queue-add-back! lq 3)		    
		    (list-queue-map!
		     (lambda (i)
		       (* i 2))
		     lq)
		    (list-queue-list lq)))

      (test-equal "list-queue-append!"
		  '(1 2 3)
		  (let ((lq1 (list-queue))
			(lq2 (list-queue)))
		    (list-queue-add-back! lq1 1)
		    (list-queue-add-back! lq1 2)
		    (list-queue-add-back! lq2 3)
		    (list-queue-list (list-queue-append! lq1 lq2)))))))
