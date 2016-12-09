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

(define (store-get name store)
  (let ((map (procedure-store-map store)))
    (or (imap-ref/default map name #f)
	(let ((record (make-procedure-record)))
	  (procedure-store-set-map! (imap-replace map name record))
	  record))))

(define-record-type <procedure-record>
  (%make-procedure-record escaping-flag)
  procedure-record?
  (escaping-flag procedure-record-escaping-flag procedure-record-set-escaping-flag!))

(define (make-procedure-record)
  (%make-procedure-record #f))

(define (escaping-procedure? name store)
  (procedure-record-escaping-flag (store-get name store)))

(define (mark-escaping-procedure! name store)
  (let ((record (store-get name store)))
    (procedure-record-set-escaping-flag! record #t)))



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

(define (make-environment)
  (%make-environment (imap eq?) (imap eq?) (imap eq?) (imap eq?) (imap eq?)))

(define (set-variable-location! variable location env)
  (environment-set-variable-locations! env
				       (imap-replace (environment-variable-locations env)
						     variable
						     location)))

(define (get-variable-location variable env)
  (imap-ref/default (environment-variable-locations env) variable #f))

(define (set-argument-registers! proc register env)
  (environment-set-argument-registers! env
				       (imap-replace (environment-argument-registers env)
						     proc
						     register)))

(define (get-argument-registers proc env)
  (imap-ref/default (environment-argument-registers env) proc #f))


(define (mark-continuation! proc env)
  (environment-set-continuation-procedures! env
					    (imap-replace (environment-continuation-procedures env)
							  proc
							  #t)))

(define (continuation-procedure? proc env)
  (imap-ref/default (environment-continuation-procedures env) proc #f))

(define (store-procedure-definition! definition env)
  (let ((name
	 (match definition
	   ((define (,name ,formal* ...) ,body)
	    name)
	   (,_ (error "invalid procedure definition" definition)))))
    (environment-set-procedure-definitions! env
					    (imap-replace (environment-procedure-definitions env)
							  name
							  definition))))

(define (procedure-definition procedure env)
  (imap-ref/default (environment-procedure-definitions env) procedure #f))
