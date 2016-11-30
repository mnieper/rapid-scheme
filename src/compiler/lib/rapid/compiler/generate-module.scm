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
  (match definitions
    (((define (,name* ,formal** ...) ,body*) ...)
     (let ((names (apply iset eq? name*)))     
       (let ((globals
	      (make-global-map
	       (apply iset-union
		      (map (lambda (formals body)
			     (extract-globals body
					      (iset-union (apply iset eq? formals)
							  names)))
			   formal** body*))))
	     (literals
	      (make-literal-map
	       (apply iset-union
		      (map extract-literals body*)))))
	 (let ((procedures
		(map (lambda (formals body)
		       (generate-procedure formals body names globals literals))
		     formal** body*)))
	   `(module ,@(map (lambda (name procedure)
			     `(procedure ,name ,@procedure))
			   name* procedures)
		    ,@(map (lambda (entry)
			     `(datum ,(cdr entry) ,(car entry)))
			   literals)
	            ,@(map (lambda (entry)
			     `(variable ,(cdr entry) ,(car entry)))
			   globals))))))
    (,_ (error "invalid definitions" definitions))))

(define (extract-globals exp locals)
  (match exp
    (,x (guard (identifier? x)) (if (iset-member? locals x)
				    (iset eq?)
				    (iset eq? x)))
    (,x (guard (literal? x)) (iset eq?))
    ((let ((,var (,operator ,(operand*) ...))) ,body)
     (let ((body (extract-globals body (iset-adjoin locals var))))
       (apply iset-union body operand*)))
    ((if ,(test) ,(consequent) ,(alternate))
     (apply iset-union test consequent alternate))
    ((,(operator) ,(operand*) ...)
     (apply iset-union operator operand*))
    (,_ (error "invalid expression" exp))))

(define (extract-literals exp)
  (match exp
    (,x (guard (identifier? x)) (iset equal?))
    (,x (guard (integer? x)) (iset equal?))
    (,x (guard (literal? x)) (iset equal? x))
    ((let ((,var (,operator ,(operand*) ...))) ,(body))
     (apply iset-union body operand*))
    ((if ,(test) ,(consequent) ,(alternate))
     (iset-union test consequent alternate))
    ((,(operator) ,(operand*) ...)
     (apply iset-union operator operand*))
    (,_ (error "invalid expression" exp))))

(define (extract-record-count exp)
  (match exp
    (,x (guard (identifier? x)) 0)
    (,x (guard (literal? x)) 0)
    ((let ((,var (make-record ,operand* ...))) ,(body))
     (+ 1 body))
    ((if ,test ,(consequent) ,(alternate))
     (+ consequent alternate))
    ((,operator ,operand* ...) 0)
    (,_ (error "invalid expression" exp))))

(define (extract-record-size exp)
  (match exp
    (,x (guard (identifier? x)) 0)
    (,x (guard (literal? x)) 0)
    ((let ((,var (make-record ,operand* ...))) ,(body))
     (+ (length operand*) body))
    ((if ,test ,(consequent) ,(alternate))
     (+ consequent alternate))
    ((,operator ,operand* ...) 0)
    (,_ (error "invalid expression" exp))))

(define (make-global-map globals)
  (iset-fold (lambda (global map)
	       (imap-replace map global (make-synthetic-identifier 'global)))
	     (make-imap eq?)
	     globals))
	  
(define (make-literal-map literals)
  (iset-fold (lambda (literal map)
	       (imap-replace map literal (make-synthetic-identifier 'literal)))
	     (make-imap eq?)
	     literals))

(define (generate-procedure formals body names globals literals)
  (let ((record-count (extract-record-count body))
	(record-size (extract-record-size body)))
    (receive (marked-body free-variables)
	(mark-free-variables body formals)
      (let ((register-map
	     (apply make-imap eq?
		    (let loop ((formals formals)
			       (registers *argument-registers*))
		      (if (null? formals)
			  '()
			  (cons (car formals)
				(cons (car registers)
				      (loop (cdr formals) (cdr registers)))))))))
	(let ((code
	       (generate-expression body register-map globals literals))
	      (live-registers
	       (map (lambda (free-variable)
		      (imap-ref register-map free-variable))
		    free-variables)))
	  `((alloc ,record-count ,record-size ,@live-registers)
	    ,@code))))))

(define (generate-expression exp register-map globals literals)
  (let loop ((exp exp) (register-map register-map))  
    (match exp
      (,x (guard (identifier? x)) (cond
				   ((imap-ref/default globals x #f)
				    => (lambda (global)
					 `(,global)))
				   ((imap-ref/default register-map x #f))
				   (else x)))
      (,x (guard (integer? x)) x)
      (,x (guard (literal? x)) `(,(imap-ref literals x)))
      ;; TODO: Literal
      ((if ,(test) ,(consequent) ,(alternate))
       (let ((consequent-label (make-synthetic-identifier 'consequent))
	     (after-if-label (make-synthetic-identifier 'after-if)))
	 `(begin (branch ,test 0 (= ,consequent-label))
		 ,alternate
		 (jump ,after-if-label)
		 ,consequent-label
		 ,consequent
		 ,after-if-label)))
      (,_ (error "invalid expression" exp)))))

(define (literal? obj)
  ;; TODO
  (or (number? obj)
      (string? obj)))
