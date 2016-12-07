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




(define (mark-free-variables exp variables)
  (receive (marked-expression variables)  
      (let loop ((exp exp) (variables (apply iset eq? variables)))
	(match exp
	  (,x (guard (identifier? x))
	      (values x
		      (if (iset-member? variables x)
			  (iset eq? x)
			  (iset eq?))))
	  ((let ((,var ,(init init-variables))) ,body)
	   (receive (body body-variables)
	       (loop body (iset-adjoin variables var))
	     (let ((variables (iset-union (iset-delete body-variables var)
					  init-variables)))
	       (values `(let (free ,@(iset->list variables)) ((,var ,init)) ,body) variables))))
	  ((if ,(test test-variables)
	       ,(consequent consequent-variables)
	       ,(alternate alternate-variables))
	   (values `(if ,test ,consequent ,alternate)
		   (iset-union test-variables consequent-variables alternate-variables)))
	  ((,(operator operator-variables) ,(operand* operand-variables*) ...)
	   (values `(,operator ,@operand*)
		   (apply iset-union operator-variables operand-variables*)))
	  (,x
	   (values x (iset eq?)))))
    (values marked-expression (iset->list variables))))

;; TODO => rewrite into iset!
(define (append-variable variables var)
  (append variables (list var)))

(define (join-variables . variables*)
  (apply append variables*))

(define (delete-variable variables var)
  (let loop ((variables variables))
    (cond
     ((null? variables)
      '())
     ((eq? var (car variables))
      (loop (cdr variables)))
     (else
      (cons (car variables) (loop (cdr variables)))))))

(define (contains-variable? variables var)
  (and (memq var variables) #t))
