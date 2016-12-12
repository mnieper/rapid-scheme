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

(define-record-type <procedure-store>
  (%make-procedure-store map)
  procedure-store?
  (map procedure-store-map procedure-store-set-map!))

(define (make-procedure-store)
  (%make-procedure-store (imap eq?)))

(define (store-get name store)
  (let ((map (procedure-store-map store)))
    (or (imap-ref/default map name #f)
	(let ((record (make-procedure-record)))
	  (procedure-store-set-map! store (imap-replace map name record))
	  record))))


(define-record-type <environment>
  (%make-environment variable-locations
		     argument-registers
		     escaping-procedures
		     continuation-procedures
		     procedure-definitions)
  environment?
  (variable-locations environment-variable-locations environment-set-variable-locations!)
  (argument-registers environment-argument-registers environment-set-argument-registers!)
  (escaping-procedures environment-escaping-procedures environment-set-escaping-procedures!)
  (continuation-procedures environment-continuation-procedures
			   environment-set-continuation-procedures!)
  (procedure-definitions environment-procedure-definitions environment-set-procedure-definitions!))

(define-record-type <procedure-record>
  (%make-procedure-record escaping-flag continuation-flag argument-registers)
  procedure-record?
  (escaping-flag procedure-record-escaping-flag procedure-record-set-escaping-flag!)
  (continuation-flag procedure-record-continuation-flag procedure-record-set-continuation-flag!)
  (definition procedure-record-definition procedure-record-set-definition!)
  (argument-registers procedure-record-argument-registers procedure-record-set-argument-registers!))

(define (make-procedure-record)
  (%make-procedure-record #f #f #f))

(define (escaping-procedure? name store)
  (procedure-record-escaping-flag (store-get name store)))

(define (mark-escaping-procedure! name store)
  (let ((record (store-get name store)))
    (procedure-record-set-escaping-flag! record #t)))

(define (continuation? name store)
  (procedure-record-continuation-flag (store-get name store)))

(define (mark-continuation! name store)
  (let ((record (store-get name store)))
    (procedure-record-set-continuation-flag! record #t)))

(define (procedure-definition name store)
  (procedure-record-definition (store-get name store)))

(define (store-procedure-definition! definition store)
  (let ((name
	 (match definition
	   ((define (,name ,formal* ...) ,body)
	    name)
	   (,_ (error "invalid procedure definition" definition)))))
    (let ((record (store-get name store)))
      (procedure-record-set-definition! record definition ))))

(define (set-argument-registers! name registers store)
  (let ((record (store-get name store)))
    (procedure-record-set-argument-registers! record registers)))

(define (get-argument-registers name store)
  (procedure-record-argument-registers (store-get name store)))
