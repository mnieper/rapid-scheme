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

(define-record-type <free-variables-environment>
  (%make-free-variables-environment map)
  free-variables-environment?
  (map free-variables-environment-map free-variables-environment-set-map!))

(define (make-free-variables-environment expr)
  (let ((env (%make-free-variables-environment (imap eq?))))
    (let loop ((expr expr) (variables (iset eq?)))
      (match expr
	(,x (guard (identifier? x)) (if (iset-member? variables x)
					(iset eq? x)
					(iset eq?)))
	((receive (,var* ...) ,(init-variables) ,body)
	 (error "receive: expr" expr) ;; FIXME
	 (receive (body-variables)
	     (loop body (iset-adjoin* variables var*))
	   (let ((variables (iset-union (iset-delete* body-variables var*)
					init-variables)))
	     variables)))
	((letrec ((,name* (lambda (,formal** ...)
			    ,body*))
		  ...)
	   ,expr)
	 (receive (variables) (iset-adjoin* variables name*)
	   (let ((expr-variables
		  (loop expr variables))
		 (body-variables*
		  (map (lambda (formals body)
			 (loop body (iset-adjoin* variables formals)))			  
		       formal** body*)))
	     (for-each (lambda (name variables)
			 (set-free-variables! name variables env))
		       name* body-variables*)		
	     (receive (free-variables)
		 (iset-delete* (apply iset-union expr-variables body-variables*)
			       name*)
	       variables))))
	((if ,(x) ,(y) ,(z)) (error "FIXME" x y z))
	((if ,(test-variables)
	     ,(consequent-variables)
	     ,(alternate-variables))
	 (iset-union test-variables consequent-variables alternate-variables))
	((,(operator-variables) ,(operand-variables*))
	 (apply iset-union operator-variables operand-variables*))
	(,_ (error "???" expr variables)
	    (iset eq?))))
    env))

(define (set-free-variables! name free-variables env)
  (free-variables-environment-set-map! env
				       (imap-replace (free-variables-environment-map env)
						     name
						     free-variables)))

(define (free-variables name env)
  (imap-ref (free-variables-environment-map env)
	    name
	    (lambda ()
	      (error "unknown procedure" name))))

(define (iset-adjoin* set obj*)
  (fold (lambda (obj set)
	  (iset-adjoin set obj))
	set
	obj*))

(define (iset-delete* set obj*)
  (fold (lambda (obj set)
	  (iset-delete set obj))
	set
	obj*))
