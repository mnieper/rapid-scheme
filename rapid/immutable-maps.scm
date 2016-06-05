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

(define-record-type <imap>
  (%imap comparator associations)
  imap?
  (comparator imap-comparator)
  (associations imap-associations))

;; TODO: Use an efficient functional data structure.

(define (imap comparator . associations)
  (let loop ((associations associations))
    (if (null? associations)
	(%imap comparator '())
	(imap-replace (loop (cddr associations))
		      (car associations)
		      (cadr associations)))))

(define imap-ref
  (case-lambda
   ((map key)
    (imap-ref map key (lambda ()
			(error "imap-ref: no value associated to key" key))))
   ((map key failure)
    (cond
     ((assoc key
	     (imap-associations map)
	     (comparator-equality-predicate (imap-comparator map)))
      => cdr)
     (else
      (failure))))))

(define (imap-ref/default map key default)
  (imap-ref map key (lambda () default)))

(define (imap-replace map key value)
  (%imap (imap-comparator map) (cons (cons key value)
				     (imap-associations map))))
