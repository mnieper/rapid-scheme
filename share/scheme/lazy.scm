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

;;; Promises

;;; XXX: Take another look at the following implementation from the report.
;;; XXX: Encapsulate promises

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-promise #f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise #t expression)))))

(define (make-promise)
  (lambda (done? proc)
    (list (cons (done? proc)))))

(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
	(unless (promise-done? promise)
	  (promise-update! promise* promise))
	(force promise))))

(define (promise-done? x)
  (caar x))

(define (promise-value x)
  (cdar x))

(define (promise-update! new old)
  (set-car! (car old) (promise-done? new))
  (set-cdr! (car old) (promise-value new))
  (set-car! new (car old)))	

(define-primitive promise? 'promise?) ; FIXME
