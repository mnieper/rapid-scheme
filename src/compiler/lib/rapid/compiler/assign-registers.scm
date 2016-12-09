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

(define-record-type <variable-environment>
  (%make-variable-environment map)
  variable-environment?
  (map variable-environment-map variable-environment-set-map!))

(define (make-variable-environment)
  (%make-variable-environment (imap eq?)))

(define (variable-location name env)
  (imap-ref (variable-environment-map env) name))

(define (set-variable-location! name register env)
  (variable-environment-set-map! env
				 (imap-replace (variable-environment-map env)
					       name register)))

(define escaping-registers
  (append (vector->list (get-callee-save-registers))
	  (vector->list (get-caller-save-registers))))

#;
(define continuation-registers
  (append (vector->list (get-callee-save-registers))
	  (vector->list (get-caller-save-registers))))

(define *registers*
  (apply iset eq? (append (vector->list (get-callee-save-registers))
			  (vector->list (get-caller-save-registers)))))

(define (default-argument-registers)
  escaping-registers)


(define (assign-registers definitions store)
  (for-each (lambda (definition)
	      (store-procedure-definition! definition store))
	    definitions)
  (let ((env (make-variable-environment)))  
    (for-each (lambda (definition)
		(%assign-registers! definition store env))
	      definitions)
    env))

(define (%assign-registers! definition store env)
  (match definition
    ((define (,name ,formal* ...) ,body)
     (unless (get-argument-registers name store)
       (let* ((registers (map (lambda (formal register)
				(set-variable-location! formal register env)
				register)
			      formal* escaping-registers)))
	 (set-argument-registers! name registers store)
	 (assign-registers-body! body store env))))
    (,_ (error "invalid definition" definition))))

(define (assign-registers-body! body store env)
  (match body
    ((if ,test ,consequent ,alternate)
     (assign-registers-body! consequent store env)
     (assign-registers-body! alternate store env))
    ((halt) #f)
    ((,operator ,operand* ...)
     (cond
      ((procedure-definition operator store)
       => (lambda (definition)
	    (match definition
	      ((define (,name ,formal* ...) ,body)
	       (unless (or (get-argument-registers name store)
			   (escaping-procedure? name store))
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
			   ((and-let* ((register (variable-location (car operands) env))
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
		   (set-argument-registers! name registers store)))))))))
    (,_ (error "invalid body" body))))
