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

(define (mark-escaping-procedures! expr store)
  (let %mark! ((expr expr) (procedures (iset eq?)))
    (let mark! ((expr expr))
      (match expr
	((if ,test ,alternate ,consequent)
	 (mark! test)
	 (mark! alternate)
	 (mark! consequent))
	((receive (,var* ...) (,operator ,operand* ...) ,expr)
	 (for-each mark! operand*)
	 (mark! expr))
	((letrec ((,name* (lambda (,formal** ...) ,body*))
		  ...)
	   ,expr)
	 (let loop ((procedures procedures) (names name*))
	   (if (null? names)
	       (let ((mark! (lambda (expr) (%mark! expr procedures))))
		 (for-each mark! body*)
		 (mark! expr))
	       (loop (iset-adjoin procedures (car names)) (cdr names)))))
	((,operator ,operand* ...)
	 (unless (iset-member? procedures operator)
	   (mark! operator))
	 (for-each mark! operand*))
	(,x (guard (iset-member? procedures x)) (mark-escaping-procedure! x store)))))
  (mark-continuations! expr store))

(define (mark-continuations! expr store)
  (let %mark! ((expr expr) (procedures (iset eq?)))
    (let mark! ((expr expr))
      (match expr
	((if ,test ,alternate ,consequent)
	 (mark! test)
	 (mark! alternate)
	 (mark! consequent))
	((receive (,var* ...) (,operator ,operand* ...) ,expr)
	 (for-each mark! operand*)
	 (mark! expr))
	((letrec ((,name* (lambda (,formal** ...) ,body*))
		  ...)
	   ,expr)
	 (let loop ((procedures procedures) (names name*))
	   (if (null? names)
	       (let ((mark! (lambda (expr) (%mark! expr procedures))))
		 (for-each mark! body*)
		 (mark! expr))
	       (loop (iset-adjoin procedures (car names)) (cdr names)))))
	((,operator ,operand* ...)
	 (when (escaping-procedure? operator store)
	   (mark-continuation! (car operand*) store))
	 (for-each mark! operand*))))))
