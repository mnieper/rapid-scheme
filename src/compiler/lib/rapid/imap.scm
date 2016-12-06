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
  (%make-imap compare alist)
  imap?
  (compare imap-compare)
  (alist imap-alist imap-set-alist!))

(define (imap->alist map)
  (list-copy (imap-alist map)))

(define (make-imap compare . items)
  (let loop ((items items))
    (if (null? items)  
	(%make-imap compare '())
	(imap-replace (loop (cddr items)) (car items) (cadr items)))))

(define imap make-imap) ;; FIXME: Remove references to make-imap

;; XXX: This does not define an immutable map because tail is modified
;; by set-cdr! below!
(define imap-ref
  (case-lambda
    ((map key)
     (define (failure)
       (error "imap-ref: key not found in map" key))
     (imap-ref map key failure))
    ((map key failure)
     (define (success value)
       value)
     (imap-ref map key failure success))
    ((map key failure success)
     (let ((compare (imap-compare map))
	   (alist (imap-alist map)))
       (let loop ((tail alist) (previous #f))	      
	 (if (null? tail)
	     (failure)
	     (let ((entry (car tail)))
	       (if (compare (car entry) key)
		   (let ((alist (if previous
				    (begin
				      (set-cdr! previous (cdr tail))
				      (cons entry alist))
				    alist)))
		     (imap-set-alist! map alist)
		     (success (cdr entry)))
		   (loop (cdr tail) tail)))))))))

(define (imap-ref/default map key default)
  (imap-ref map key (lambda () default)))

(define (imap-replace map key value)
  (let ((compare (imap-compare map)))
    (define (failure)
      (%make-imap compare (cons (cons key value) (imap-alist map))))
    (define (success old-value)
      (%make-imap compare (cons (cons key value) (cdr (imap-alist map)))))
    (imap-ref map key failure success)))

(define (imap-for-each proc map)
  (for-each (lambda (entry)
	      (proc (car entry) (cdr entry)))
	    (imap-alist map)))
