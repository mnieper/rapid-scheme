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

(define-record-type <environment>
  (%make-environment variable-locations argument-registers)
  environment?
  (variable-locations environment-variable-locations environment-set-variable-locations!)
  (argument-registers environment-argument-registers environment-set-argument-registers!))

(define (make-environment)
  (%make-environment (imap eq?) (imap eq?)))

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
