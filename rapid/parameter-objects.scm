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

(define <param> (vector #f))

(define make-parameter
  (case-lambda
   ((init)
    (make-parameter init (lambda (value) value)))
   ((init converter)
    (let ((scheme-parameter (scheme-make-parameter init
						   (lambda (value)
						     (box (converter value))))))
      (case-lambda
       (()
	(unbox (scheme-parameter)))
       ((value)
	(if (eq? value <param>)
	    scheme-parameter	
	    (set-box! (scheme-parameter) (converter value))))
       (else
	(error "bad parameter syntax")))))))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((param value) ...) . body)
     (scheme-parameterize (((param <param>) value) ...) . body))))
