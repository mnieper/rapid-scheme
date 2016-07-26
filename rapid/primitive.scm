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

;; Fundamental binding construct

(define-syntax letrec*-values
  (syntax-rules ()
    ((letrec*-values ((formals init) ...) body1 body2 ...)
     (let ()
       (define-values formals init)
       ...
       (let ()
	 body1
	 body2
	 ...)))))

;; Set multiple values

(define-syntax set!-values
  (syntax-rules ()
    ((set!-values formals expression)
     (set!-values-aux formals () () expression))))

(define-syntax set!-values-aux
  (syntax-rules ()
    ((set!-values-aux () new-formals ((var new) ...) expression)
     (let-values ((new-formals expression))
       (set! var new)
       ...))
    ((set!-values-aux (var . formals) (new-formal ...) (tmp ...) expression)
     (set!-values-aux formals (new-formal ... new) (tmp ... (var new)) expression))
    ((set!-values-aux var new-formals (tmp ...) expression)
     (set!-values-aux () (new-formals . var) (tmp ... (var new)) expression))))

;; Disjoint types

(define-record-type <type>
  (make-type payload)
  type?
  (payload type-payload type-set-payload!))

(define-record-type <instance>
  (make-instance type payload)
  %instance?
  (type instance-type)
  (payload instance-ref))

(define (instance? type obj)
  (and (%instance? obj)
       (eq? type (instance-type obj))))
