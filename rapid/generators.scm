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

(define (make-coroutine-generator proc)
  (define return #f)
  (define resume #f)
  (define (yield value)
    (call-with-current-continuation
     (lambda (cc)
       (set! resume cc)
       (return value))))
  (lambda ()
    (call-with-current-continuation
     (lambda (cc)
       (set! return cc)
       (cond
	(resume
	 (resume #f))
	(else
	 (proc yield)
	 (set! resume (lambda (value) (return (eof-object))))
	 (return (eof-object))))))))

(define make-range-generator
  (case-lambda
   ((start)
    (make-range-generator start +inf.0 1))
   ((start end)
    (make-range-generator start end 1))
   ((start end step)
    (make-coroutine-generator
     (lambda (yield)
       (do ((value start (+ value step)))
	   ((>= value end))
	 (yield value)))))))

(define (gappend . gen*)
  (make-coroutine-generator
   (lambda (yield)
     (do ((gen* gen* (cdr gen*)))
	 ((null? gen*))
       (do ((value ((car gen*)) ((car gen*))))
	   ((eof-object? value))
	 (yield value))))))

(define (generator-fold proc seed . gen*)
  (call-with-current-continuation
   (lambda (return)
     (let loop ((result seed))
       (loop (apply proc
		    (let loop ((gen* gen*))
		      (if (null? gen*)
			  (list result)
			  (let ((value ((car gen*))))
			    (if (eof-object? value)
				(return result)
				(cons value (loop (cdr gen*)))))))))))))

(define generator->list
  (case-lambda
   ((generator) (generator->list generator +inf.0))
   ((generator k)
    (let loop ((i 0))
      (if (= i k)
	  '()
	  (let ((value (generator)))
	    (if (eof-object? value)
		'()
		(cons value (loop (+ i 1))))))))))
