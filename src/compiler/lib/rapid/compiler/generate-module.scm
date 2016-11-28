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
     (receive (procedures literals globals)
	 (let loop ((names name*) (formal** formal**) (body* body*))
	   (if (null? names)
	       (values '() '() '())
	       (receive (procedures literals globals)
		   (loop (cdr names) (cdr formal**) (cdr body*))
		 (let ((name (car names)) (formals (car formal**)) (body (car body*)))
		   (receive (code literals globals)
		       (generate-procedure formals body names literals globals)
		     (values (cons `(procedure ,name ,@code) procedures)
			     literals
			     globals))))))	       
       `(module ,@procedures ,@literals ,@globals)))
    (,_ (error "invalid definitions" definitions))))

(define (generate-procedure formals body names literals globals)
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
      (receive (code literals globals record-count record-size)
	  (generate-expression body register-map names literals globals)
	(let ((live-registers
	       (map (lambda (free-variable)
		      (imap-ref register-map free-variable))
		    free-variables)))
	  (values `((alloc ,record-count ,record-size ,@live-registers)
		    ,@code)
		  literals globals))))))


;; returns a module expression/statement
;; and literals / globals / record-count / record-size
(define (generate-expression exp register-map names literals globals)
  (match exp
    (,x (guard (identifier? x))   ;; TODO: NAMES LITERALS GLOBALS?
     (values (imap-ref register-map x) literals globals 0 0))
    (,x (guard (integer? x))
     (values x literals globals 0 0))
    ((if ,(x xl xg xc xs) ,(y yl yg yc ys) ,(z zl zg zc zs))
     (let ((consequent-label (make-synthetic-identifier 'consequent))
	   (after-if-label (make-synthetic-identifier 'after-if)))
       (values
	`(begin (branch ,x 0 (= ,consequent-label))
		,z
		(jump ,after-if-label)
		,consequent-label
		,y
		,after-if-label)
	;; Problem: vvv how often do we count literals?
	'TODO #;(join-literals xl yl zl) (join-globals xg yg zg) (+ xc yc zc) (+ xs ys zs))))
    
	
     
    (,_ (error "invalid expression" exp))))

(define (join-globals . globals*)
  ;; FIXME
  (apply append globals*))
