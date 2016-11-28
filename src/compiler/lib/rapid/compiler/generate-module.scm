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

(define *argument-registers*
  (let ((caller-save-registers (vector->list (get-caller-save-registers)))
	(callee-save-registers (vector->list (get-callee-save-registers))))
    `(,(car caller-save-registers)      ; Closure
      ,(cadr caller-save-registers)     ; Continuation
      ,@callee-save-registers           ; Continuation parameters
      ,@(cddr caller-save-registers)))) ; Proper arguments

(define (generate-module definitions)
  ;; TODO: match definitions one by one
  (match definitions
    (((define (,name* ,formal**) ,body*) ...)
     (receive (procedures literals globals)
	 (let loop ((names name*) (formal** formal**) (body* body*))
	   (if (null? names)
	       (values '() '() '())
	       (receive (procedures literals globals)
		   (loop (cdr names) (cdr formal**) (cdr body*))
		 (let ((name (car names)) (formals (car formal**)) (body (car body*)))
		   (receive (code literals globals)
		       (generate-procedure formals body names literals globals)
		     (values (cons `(,name ,code) procedures)
			     literals
			     globals))))))	       
       `(module ,@procedures ,@literals ,@globals)))
    (,_ (error "invalid definitions" definitions))))


;; what is a procedure?
;;
;; (P u1 u2 u3 u4 c)   => leaf
;; (let ((a (+ 0 9 0)))
;;   (


(define (generate-procedure formals body names literals globals)
  (let ((register-map (map (lambda (formal register)
			     (list formal register))
			   formals *argument-registers*)))
    ;; need to know registers that are in use... HOW?
    ;; compile subexpression. It tells us which are free and how many records are alloc'd.
    ;; we need to know which vars are free ...
    ;; how? where to mark the free variables? (where they are bound!!!)
    (values 'TODO 'TODO 'TODO)))

  
  ;; during generation of the procedure
  ;; registers are chosen so that a table registers->locals is generated
  
  
#;(define (mark-free-vars exp vars)
  (receive (exp free-vars)
      (let loop ((exp exp))
	(match exp
	  ()))
    exp)) ;; or exp & free-vars (to see which are needed at procedure entry...)
      


;; TODO: Write procedure that marks free vars

