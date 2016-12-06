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

(define (generate-module definitions env)
  (let ((literals (get-literals* definitions env))
	(globals (get-globals* definitions env)))
    (let ((procedures (generate-procedures definitions env literals globals)))
      `(module ,@procedures
	       ,@(generate-literals literals)
	       ,@(generate-globals globals)))))

(define (generate-literals literals)
  (map (lambda (entry)
	 (let ((literal (car entry))
	       (name (cdr entry)))
	   `(datum ,name ,literal))) ;; TODO: Maybe we have to encode the literal (e.g. pairs)
       (imap->alist literals)))

(define (generate-globals globals)
  (map (lambda (entry)
	 (let ((global (car entry))
	       (name (cdr entry)))
	   `(variable ,name ,global)))
       (imap->alist globals)))

(define (generate-procedures definitions env literals globals)
  (map (lambda (definition)
	 (generate-procedure definition env literals globals))
       definitions))

(define (generate-procedure definition env literals globals)
  (match definition
    ((define (,name ,formal* ...) ,body)
     (let ((record-count (get-record-count body))
	   (record-size (get-record-size body))
	   (body (generate-body body env literals globals)))
       `(procedure ,name
		   (alloc ,record-count ,record-size ,(get-argument-registers name env))
		   ,@body)))))

(define (generate-body body env literals globals)
  (let ((generate-expression
	 (lambda (exp)
	   (generate-expression exp env literals globals))))  
    (match body
      ((if ,(generate-expression -> test) ,(consequent) ,(alternate))
       (let ((consequent-label (make-synthetic-identifier 'consequent))
	     (after-if-label (make-synthetic-identifier 'after-if)))
	 `((branch ,test 0 (= ,consequent-label))
	   ,alternate
	   (jump ,after-if-label)
	   ,consequent-label
	   ,consequent
	   ,after-if-label)))
      ((,operator ,(generate-expression -> operand*) ...)
       (let ((target-registers (get-argument-registers operator env)))
	 (let ((operator (generate-expression operator)))
	   `(,@(parallel-move* operand* target-registers)
	     (jump ,operator)))))
      (,_ (error "invalid body" body)))))

(define (generate-expression exp env literals globals)
  (cond
   ((number? exp) exp)
   ((literal-label exp literals))
   ((global-label exp globals))
   ((get-variable-location exp env))
   (else
    (error "invalid expression" exp))))

(define (get-globals* definitions env)
  (match definitions
    (((define (,name* ,formal** ...) ,body*) ...)
     (let ((names (apply iset eq? name*)))  
       (let ((globals (apply iset-union
			     (map (lambda (formals body)
				    (get-globals body
						 (iset-union (apply iset eq? formals)
							     names)))
				  formal** body*))))
	 (make-global-map globals))))
    (,_ (error "invalid definitions" definitions))))

(define (get-literals* definitions env)
  (match definitions
    (((define (,name* ,formal** ...) ,body*) ...)
     (let ((literals (apply iset-union
			    (map get-literals body*))))
       (make-literal-map literals)))
    (,_ (error "invalid definitions" definitions))))

(define (make-global-map globals)
  (iset-fold (lambda (global map)
	       (imap-replace map global (make-synthetic-identifier 'global)))
	     (make-imap eq?)
	     globals))
	  
(define (make-literal-map literals)
  (iset-fold (lambda (literal map)
	       (imap-replace map literal (make-synthetic-identifier 'literal)))
	     (make-imap equal?)
	     literals))

(define (global-label global globals)
  (imap-ref/default globals global #f))

(define (literal-label literal literals)
  (imap-ref/default literals literal #f))

(define (get-globals exp locals)
  (match exp
    (,x (guard (identifier? x)) (if (iset-member? locals x)
				    (iset eq?)
				    (iset eq? x)))
    (,x (guard (literal? x)) (iset eq?))
    ((receive (,var* ...) (,operator ,(operand*) ...) ,body)
     (let ((body (get-globals body (iset-union locals (apply iset eq? var*)))))
       (apply iset-union body operand*)))
    ((if ,(test) ,(consequent) ,(alternate))
     (apply iset-union test consequent alternate))
    ((,(operator) ,(operand*) ...)
     (apply iset-union operator operand*))
    (,_ (error "invalid expression" exp))))

(define (get-literals exp)
  (match exp
    (,x (guard (identifier? x)) (iset equal?))
    (,x (guard (integer? x)) (iset equal?))
    (,x (guard (literal? x)) (iset equal? x))
    ((receive (,var* ...) (,operator ,(operand*) ...) ,(body))
     (apply iset-union body operand*))
    ((if ,(test) ,(consequent) ,(alternate))
     (iset-union test consequent alternate))
    ((,(operator) ,(operand*) ...)
     (apply iset-union operator operand*))
    (,_ (error "invalid expression" exp))))

(define (get-record-count exp)
  (match exp
    (,x (guard (identifier? x)) 0)
    (,x (guard (literal? x)) 0)
    ((receive (,var) (make-record ,operand* ...) ,(body))
     (+ 1 body))
    ((receive (,var* ...) (,operator ,operand* ...) ,(body))
     body)
    ((if ,test ,(consequent) ,(alternate))
     (+ consequent alternate))
    ((,operator ,operand* ...) 0)
    (,_ (error "invalid expression" exp))))

(define (get-record-size exp)
  (match exp
    (,x (guard (identifier? x)) 0)
    (,x (guard (literal? x)) 0)
    ((receive (,var) (make-record ,operand* ...) ,(body))
     (+ (length operand*) body))
    ((receive (,var* ...) (,operator ,operand* ...) ,(body))
     body)
    ((if ,test ,(consequent) ,(alternate))
     (+ consequent alternate))
    ((,operator ,operand* ...) 0)
    (,_ (error "invalid expression" exp))))

#;(define (generate-body formals body names globals literals)
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

#;(define (generate-expression exp register-map globals literals)
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
      ((halt) '((halt)))
      ((if ,(test) ,(consequent) ,(alternate))
       (let ((consequent-label (make-synthetic-identifier 'consequent))
	     (after-if-label (make-synthetic-identifier 'after-if)))
	 `((branch ,test 0 (= ,consequent-label))
	   ,alternate
	   (jump ,after-if-label)
	   ,consequent-label
	   ,consequent
	   ,after-if-label)))
      ((,(operator) ,(operand*) ...)
       `(,@(multiple-move operand* *argument-registers*)
	 (jump ,operator)))
      (,_ (error "invalid expression" exp)))))


(define (literal? obj)
  ;; FIXME
  (pair? obj)
  (string? obj))
