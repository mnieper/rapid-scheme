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

(define-record-type <synthetic-identifier>
  (%make-synthetic-identifier name counter)
  synthetic-identifier?
  (name synthetic-identifier-name)
  (counter %synthetic-identifier-counter synthetic-identifier-set-counter!))

(define (make-synthetic-identifier name)
  (%make-synthetic-identifier name #f))

(define (identifier? obj)
  (or (symbol? obj) (synthetic-identifier? obj)))

(define (identifier-name identifier)
  (if (symbol? identifier)
      identifier
      (identifier-name (synthetic-identifier-name identifier))))

(define (synthetic-identifier-counter synthetic-identifier)
  (let ((counter (%synthetic-identifier-counter synthetic-identifier)))
    (or counter
        (let ((counter (get-counter-value (identifier-name synthetic-identifier))))
          (synthetic-identifier-set-counter! synthetic-identifier counter)
          counter))))      

(define (identifier->symbol identifier)
  (if (symbol? identifier)
      identifier
      (let ((counter (synthetic-identifier-counter identifier)))
        (string->symbol (string-append (symbol->string (identifier-name identifier))
                                       (number->string counter))))))

(define *counters* '())
(define (get-counter-value name)
  (cond
   ((assq name *counters*)
    => (lambda (counter)
         (let ((value (cdr counter)))
           (set-cdr! counter (+ value 1))
           value)))
   (else
    (let ((counter (cons name 1)))
      (set! *counters* (cons counter *counters*))
      0))))

(define (make-renaming-procedure)
  (define *renames* '())
  (define (rename identifier)
    (cond
     ((assq identifier *renames*)
      => cdr)
     (else
      (let ((synthetic-identifier (make-synthetic-identifier identifier)))
        (set! *renames* (cons (cons identifier synthetic-identifier) *renames*))
        synthetic-identifier))))
  rename)
