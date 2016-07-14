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

;;; Procedures

(define-syntax lambda
  (syntax-rules ()
    ((lambda formals body1 body2 ...)
     (case-lambda
      (formals body1 body2 ...)))
    ((lambda . _)
     (syntax-error "bad lambda syntax"))))

;;; Definitions

(define-syntax define
  (syntax-rules ()
    ((define (variable . formals) body1 body2 ...)
     (define-values (variable)
       (lambda formals body1 body2 ...)))
    ((define variable expression)
     (define-values (variable) expression))
    ((define . args)
     (syntax-error "bad define syntax"))))

;;; Conditionals

(define-syntax else
  (syntax-rules ()
    ((else . _)
     (syntax-error "invalid use of auxiliary syntax ‘else’"))))

(define-syntax =>
  (syntax-rules ()
    ((=> . _)
     (syntax-error "invalid use of auxiliary syntax ‘else’"))))

(define-syntax cond
  (syntax-rules ... (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))
    ((cond . _)
     (syntax-error "bad cond syntax"))))

(define-syntax and
  (syntax-rules ... ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ... ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ... (if #f #f))))
    ((when . _)
     (syntax-error "bad when syntax"))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ... (if #f #f))))))

(define-syntax case
   (syntax-rules ... (else =>)
     ((case (key ...)
        clauses ...)
      (let ((atom-key (key ...)))
        (case atom-key clauses ...)))
     ((case key
        (else => result))
      (result key))
     ((case key
        (else result1 result2 ...))
      (begin result1 result2 ...))
     ((case key
        ((atoms ...) => result))
      (if (memv key '(atoms ...))
          (result key)))
     ((case key
        ((atoms ...) => result)
        clause clauses ...)
      (if (memv key '(atoms ...))
          (result key)
          (case key clause clauses ...)))
     ((case key
        ((atoms ...) result1 result2 ...))
      (if (memv key '(atoms ...))
          (begin result1 result2 ...)))
     ((case key
        ((atoms ...) result1 result2 ...)
        clause clauses ...)
      (if (memv key '(atoms ...))
          (begin result1 result2 ...)
          (case key clause clauses ...)))))

;;; Iteration

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...) (test expr ...) command ...)
     (let loop ((var init) ...)
       (cond
        (test
         (if #f #f)
         expr ...)
        (else
         command ...
         (loop (do-aux var step ...) ...)))))
    ((do . _)
     (syntax-error "bad do syntax"))))

(define-syntax do-aux
  (syntax-rules ()
    ((do-aux x)
     x)
    ((do-aux x y)
     y)))

;;; Exception handling

;; FIXME: Apply R7RS erratum 17.
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call-with-current-continuation
       (lambda (guard-k)
	 (with-exception-handler
	  (lambda (condition)
	    ((call-with-current-continuation
	      (lambda (handler-k)
		(guard-k
		 (lambda ()
		   (let ((var condition))
		     (guard-aux
		      (handler-k
		       (lambda ()
			 (raise-continuable condition)))
		      clause ...))))))))
	  (lambda ()
	    (call-with-values
		(lambda () e1 e2 ...)
	      (lambda args
		(guard-k
		 (lambda ()
		   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules ... (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
	   (result temp)
	   reraise)))
    ((guard-aux reraise (test => result)
		clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   (result temp)
	   (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   temp
	   (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
	 (begin result1 result2 ...)
	 reraise))
    ((guard-aux reraise
		(test result1 result2 ...)
		clause1 clause2 ...)
     (if test
	 (begin result1 result2 ...)
	 (guard-aux reraise clause1 clause2 ...)))))

;;; Binding constructs

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))
    ((let . _)
     (syntax-error "bad let syntax"))))

(define-syntax let*
  (syntax-rules ... ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))
    ((let* . _)
     (syntax-error "bad let* syntax"))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((variable init) ...) body1 body2 ...)
     (let ()
       (define-values (variable ...) (values init ...))
       (let ()
         body1 body2 ...)))
    ((letrec . _)
     (syntax-error "bad letrec syntax"))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec ((variable init) ...) body1 body2 ...)
     (let ()
       (define-values (variable) init) ...
       (let ()
         body1 body2 ...)))
    ((letrec . _)
     (syntax-error "bad letrec* syntax"))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values ((formals init) ...) body1 body2 ...)
     (let-values-aux ((formals init) ...) () (body1 body2 ...)))
    ((let-values . _)
     (syntax-error "bad let-values syntax"))))
  
(define-syntax let-values-aux
  (syntax-rules ... ()
    ((let-values-aux () ((formals init tmp) ...) body)
     (let ()
       (define-values tmp init)
       ...
       (let ()
         (define-values formals (apply values tmp))
         ...
         (let () . body))))
    ((let-values-aux ((formals init) . bindings) tmps body)
     (let-values-aux bindings ((formals init tmp) . tmps) body))))

(define-syntax let*-values
  (syntax-rules ... ()
    ((let*-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let*-values ((formals init) . bindings) body1 body2 ...)
     (let-values ((formals init))
       (let*-values bindings body1 body2 ...)))
    ((let*-values . _)
     (syntax-error "bad let*-values syntax"))))

;;; Macros

(define-syntax let-syntax
  (syntax-rules ()
    ((let-syntax ((keyword spec) ...) body1 body2 ...)
     (let-syntax-aux ((keyword spec) ...) () (body1 body2 ...)))
    ((let-syntax . _)
     (syntax-error "bad let-syntax syntax"))))

(define-syntax let-syntax-aux
  (syntax-rules ... ()
    ((let-syntax-aux () ((tmp keyword spec) ...) body)
     (letrec-syntax ((tmp spec) ...)
       (letrec-syntax ((keyword
			(syntax-rules ()
			  ((_ . args) (tmp . args))))
		       ...)
	 . body)))
    ((let-syntax-aux ((keyword spec) . rest) (transformed ...) body)
     (let-syntax-aux rest (transformed ... (tmp keyword spec)) body))))

(define-syntax letrec-syntax
  (syntax-rules ()
    ((letrec-syntax ((keyword spec) ...) body1 body2 ...)
     (let ()
       (define-syntax keyword spec) ...
       body1 body2 ...))))

;;; Quasiquotation

(define-syntax unquote
  (syntax-rules ()
    ((unquote . args)
     (syntax-error "invalid use of auxiliary syntax ‘unquote’"))))

(define-syntax unquote-splicing
  (syntax-rules ()
    ((unquote-splicing . args)
     (syntax-error "invalid use of auxiliary syntax ‘unquote-splicing’"))))

(define-syntax quasiquote
  (syntax-rules ()
    ((quasiquote template)
     (quasiquote-aux template))
    ((quasiquote . args)
     (syntax-error "bad quasiquotation"))))

(define-syntax quasiquote-aux
  (syntax-rules ... (quasiquote unquote unquote-splicing)
    ((quasiquote-aux ,form)
     form)   
    ((quasiquote-aux (,@form . rest))
     (append form (quasiquote rest)))
    ((quasiquote-aux `form . depth)
     (list 'quasiquote (quasiquote-aux form #f . depth)))
    ((quasiquote-aux ,form #f . depth)
     (list 'unquote (quasiquote-aux form . depth)))
    ((quasiquote-aux ,@form x . depth)
     (list 'unquote-splicing (quasiquote-aux form . depth)))
    ((quasiquote-aux (car . cdr) . depth)
     (cons (quasiquote-aux car . depth) (quasiquote-aux cdr . depth)))
    ((quasiquote-aux #(element ...) . depth)
     (list->vector (quasiquote-aux (element ...) . depth)))
    ((quasiquote-aux constant . depth)
     'constant)))

;;; Parameter objects

(define make-parameter
  (case-lambda
   ((init)
    (make-parameter init (lambda (value) value)))
   ((init converter)
    (define value (converter init))
    (lambda args
      (cond
       ((null? args)
        value)
       ((eq? (car args) <param-set!>)
        (set! value (cadr args)))
       ((eq? (car args) <param-convert>)
        converter)
       (else
        (error "bad parameter syntax")))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((param value) ...) body1 body2 ...)
     (parameterize-aux () ((param value) ...) (body1 body2 ...)))
    ((parameterize . args)
     (syntax-error "bad parameterize syntax"))))

(define-syntax parameterize-aux
  (syntax-rules ... ()
    ((parameterize-aux
         ((param value p old new) ...)
         ()
         body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new ((p <param-convert>) value)) ...)
         (dynamic-wind
             (lambda () (p <param-set!> new) ...)
             (lambda () . body)
             (lambda () (p <param-set!> old) ...)))))
    ((parameterize-aux
      args
      ((param value) . rest)
      body)
     (parameterize-aux
      ((param value p old new) . args)
      rest
      body))))

(define <param-set!> (vector #f))
(define <param-convert> (vector #f))

;;; Input and output

(define current-input-port (make-parameter (%current-input-port)))
(define current-output-port (make-parameter (%current-output-port)))
(define current-error-port (make-parameter (%current-error-port)))

;;; Input

(define read-char
  (case-lambda
   (() (%read-char (current-input-port)))
   ((port) (%read-char port))))

(define peek-char
  (case-lambda
   (() (%peek-char (current-input-port)))
   ((port) (%peek-char port))))

(define peek-u8
  (case-lambda
   (() (%peek-u8 (current-input-port)))
   ((port) (%peek-u8 port))))

(define read-line
  (case-lambda
   (() (%read-line (current-input-port)))
   ((port) (%read-line port))))

(define char-ready?
  (case-lambda
   (() (%char-ready? (current-input-port)))
   ((port) (%char-ready? port))))

(define read-string
  (case-lambda
   ((k) (%read-string k (current-input-port)))
   ((k port) (%read-string k port))))

(define read-u8
  (case-lambda
   (() (%read-u8 (current-input-port)))
   ((port) (%read-u8 port))))

(define u8-ready?
  (case-lambda
   (() (%u8-ready? (current-input-port)))
   ((port) (%u8-ready? port))))

(define read-bytevector
  (case-lambda
   ((k) (%read-bytevector (current-input-port)))
   ((k port) (%read-bytevector k port))))

(define read-bytevector!
  (case-lambda
   ((bytevector) (%read-bytevector! bytevector (current-input-port)))
   ((bytevector port) (%read-bytevector! bytevector port))
   ((bytevector port start) (%read-bytevector! bytevector port start))
   ((bytevector port start end) (%read-bytevector! bytevector port start end))))

;;; Output

(define newline
  (case-lambda
   (() (%newline (current-output-port)))
   ((port) (%newline port))))

(define write-char
  (case-lambda
   ((char) (%write-char char (current-output-port)))
   ((char port) (%write-char char port))))

(define write-string
  (case-lambda
   ((string) (%write-string string (current-output-port)))
   ((string port) (%write-string string port))
   ((string port start) (%write-string port start))
   ((string port start end) (%write-string port start end))))

(define write-u8
  (case-lambda
   ((byte) (%write-u8 byte (current-output-port)))
   ((byte port) (%write-u8 byte port))))

(define write-bytevector
  (case-lambda
   ((bytevector) (%write-bytevector bytevector (current-output-port)))
   ((bytevector port) (%write-bytevector bytevector port))
   ((bytevector port start) (%write-bytevector port start))
   ((bytevector port start end) (%write-bytevector port start end))))

(define flush-output-port
  (case-lambda
   (() (%flush-output-port (current-output-port)))
   ((port) (%flush-output-port port))))

;;; Features

(define (features)
  (rapid-features))
