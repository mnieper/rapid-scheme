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

(define escaping-registers
  (append (vector->list (get-callee-save-registers))
	  (vector->list (get-caller-save-registers))))

(define continuation-registers
  (append (vector->list (get-callee-save-registers))
	  (vector->list (get-caller-save-registers))))

(define *registers*
  (apply iset eq? (append (vector->list (get-callee-save-registers))
			  (vector->list (get-caller-save-registers)))))

(define (default-argument-registers)
  escaping-registers)

(define (assign-registers! definitions env)
  (for-each (lambda (definition)
	      (store-procedure-definition! definition env))
	    definitions)
  (for-each (lambda (definition)
	      (%assign-registers! definition env))
	    definitions))

(define (%assign-registers! definition env)
  (match definition
    ((define (,name ,formal* ...) ,body)
     (unless (get-argument-registers name env)
       (let* ((registers (if (continuation-procedure? name env)
			    continuation-registers
			    escaping-registers))
	      (registers (map (lambda (formal register)
				(set-variable-location! formal register env)
				register)
			      formal* registers)))
	 (set-argument-registers! name registers env)
	 (assign-registers-body! body env))))
    (,_ (error "invalid definition" definition))))

(define (assign-registers-body! body env)
  (match body
    ((if ,test ,consequent ,alternate)
     (assign-registers-body! consequent env)
     (assign-registers-body! alternate env))
    ((halt) #f)
    ((,operator ,operand* ...)
     (cond
      ((procedure-definition operator env)
       => (lambda (definition)
	    (match definition
	      ((define (,name ,formal* ...) ,body)
	       (unless (or (get-argument-registers name env)
			   (escaping-procedure? name env))
		 (let*
		     ((available-registers
		       (let loop ((registers *registers*) (operands operand*))
			 (if (null? operands)
			     registers
			     (loop (iset-delete registers (car operands))
				   (cdr operands)))))
		      (registers
			(let loop ((formals formal*)
				   (operands operand*)
				   (used-registers (iset eq?))
				   (registers (iset->list available-registers)))
			  (cond
			   ((null? formals)
			    '())
			   ((and-let* ((register (get-variable-location (car operands) env))
				       ((not (iset-member? used-registers register))))
			      register)
			    => (lambda (register)			      
				 (cons register (loop (cdr formals)
						      (cdr operands)
						      (iset-adjoin used-registers register)
						      registers))))
			   (else
			    (cons (car available-registers)
				  (loop (cdr formals)
					(cdr operands)
					used-registers
					(cdr registers))))))))
		   (set-argument-registers! name registers env)))))))))
    (,_ (error "invalid body" body))))
