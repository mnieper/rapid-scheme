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

;; Module language
;;
;; <input> : (reg <register-name>)
;;         | (const <constant-value>) | (global <global-name>) | (ref <register-name> <offset>) | (byte-ref <register-name> <offset>) | (local <local-name>)
;; <offset>: (reg <register-name>) | (const <constant-value>)
;;
;; (assign <register-name> <input>)
;; (assign <register-name> (op <operation-name>) <input> ...)
;; (perform (op <operation-name>) <input> ...)
;; (test (op <operation-name) <input> ...)
;; (branch (label <label>))
;; (goto (label <label>))
;; (goto (reg <register-name>))
;; (goto (ref <register-name> <offset>))
;; (call <global-name>)
;; (set! <register name> <offset> <offset>)
;; (byte-set! <register name> <offset> <offset>)
;; (assign <register-name> (record ...))


;;; Modules

(define-record-type <module>
  (%make-module offsets code)
  module?
  (offsets module-offsets)
  (code module-code))

(define (make-module module)
 (match module
   ((module
     (procedures (,proc-name* ,proc-code*) ...)
     (data (,data-name* ,data-bytes*) ...)
     (variables (,var-name* ,var-init*) ...))
    (compile-module (map list proc-name* proc-code*)
   	  	    (map list data-name* data-bytes*)
		    (map list var-name* var-init*)))))

(define (compile-module procedures datums vars)
  (let ((start-label (make-synthetic-identifier 'start))
	(end-label (make-synthetic-identifier 'end)))
    
    (define (compile-procedures procedures)
      `(begin ,@(map compile-procedure procedures)))

    (define (compile-datums datums)
      `(begin ,@(map compile-datum datums)))

    (define (compile-vars vars)
      `(begin ,@(map compile-var vars)))

    (define (compile-procedure procedure)
      (let ((label (car procedure))
	    (stmts (cadr procedure)))
	`(begin (align 8)
		,label
		,(compile-statements stmts))))

    (define (compile-datum datum)
      (let ((label (car datum))
	    (bytes (cadr datum)))
	`(begin (align 8)
		(quad (- ,label ,start-label)) ;; GC-FLAG
		,label
		,(bytevector->assembly bytes))))

    (define (compile-var var)
      (let ((label (car var))
	    (init (cadr var)))
	`(begin (align 8)
		,label
		(quad ,init))))

    (define (compile-statements stmts)
      `(begin ,@(map compile-statement stmts)))

    (define (compile-statement stmt)
      (cond
       ((label? stmt)
	(compile-label stmt))
       ((instruction? stmt)
	(compile-instruction stmt))
       (else
	(error "invalid statement" stmt))))

    (define (compile-instruction inst)
      (case (instruction-name inst)
	((call)
	 (compile-call inst))
	((assign)
	 (compile-assignment inst))
	((halt)
	 (compile-halt inst))
	(else (error "invalid instruction" inst))))

    (define (compile-reference exp)
      (cond
       ((register? exp)
	(get-machine-register (register-index exp)))
       ((global? exp)
	(global-symbol exp))
       (else
	(error "invalid reference" exp))))
    
    (define (compile-expression exp target)
      (cond
       ((register? exp)
	`(movq ,(get-machine-register (register-index exp)) ,(compile-reference target)))
       ((immediate? exp)
	`(movq ,(immediate-value exp) ,(compile-reference target)))
       ((global? exp)
	(if (register? target)
	    `(movq ,(global-symbol exp) ,(compile-reference target))
	    `(begin (movq ,(global-symbol exp) rax)
		    (movq rax ,(compile-reference target)))))
       ((label? exp)
	(let ((after-instruction-label (make-synthetic-identifier 'after-instruction-label)))	    
	  (if (register? target)
	      `(begin (leaq ((- ,exp ,after-instruction-label) rip) ,(compile-reference target))
		      ,after-instruction-label)
	      `(begin (leaq ((- ,exp ,after-instruction-label) rip) rax)
		      ,after-instruction-label
		      (movq rax ,(compile-reference target))))))
       (else
	(error "invalid expression" exp))))
    
    (define (compile-assignment inst)
      (let ((target (assignment-target inst))
	    (source (assignment-source inst)))
	(compile-expression source target)))

    (let ((procedures-assembly (compile-procedures procedures))
	  (datums-assembly (compile-datums datums))
	  (vars-assembly (compile-vars vars)))
      (let-values (((code offsets)
		    (assemble `(begin ,start-label
				      (quad (- ,end-label ,start-label))
				      (quad ,(* 8 (length vars)))
				      ,procedures-assembly
				      ,datums-assembly
				      ,vars-assembly
				      ,end-label))))
	(let ((offsets (filter offsets (append (map car procedures)
					       (map car datums)
					       (map car vars)))))
	  (%make-module offsets code))))))

(define (module-label-offset module label)
  (imap-ref (module-offsets module) label))

(define (module-reference module label)
  (make-reference module (module-label-offset module label)))

(define (bytevector->assembly bytes)
  `(begin ,@(let loop ((i 0))
	      (if (= i (bytevector-length bytes))
		  '()
		  (cons `(byte ,(bytevector-u8-ref bytes i))
			(loop (+ i 1)))))))

(define (filter map keys)
  (let loop ((filtered-map (make-imap eq?)) (keys keys))
    (if (null? keys)
	filtered-map
	(loop (imap-replace filtered-map (car keys)
			    (imap-ref map (car keys)))
	      (cdr keys)))))

(define (global-symbol symbol)
  (let ((index (global-symbol-index symbol)))
    `(,(* index 8) rbp))) 

(define (label? stmt)
  (identifier? stmt))

(define (instruction? stmt)
  (pair? stmt))

(define (instruction-name inst)
  (car inst))

(define (instruction-operands inst)
  (cdr inst))

(define (compile-label label)
  label)

(define (call-instruction-callee inst)
  (cadr inst))

(define (assignment-target inst)
  (cadr inst))

(define (assignment-source inst)
  (caddr inst))

(define (compile-call inst)
  (let ((callee (call-instruction-callee inst)))
    (case callee
      ((exit)
       `(begin (sarq rdi)
	       (callq ,(global-symbol 'exit))))
      ((fputs)
       `(callq ,(global-symbol 'fputs)))
      (else
       (error "unknown procedure" callee)))))

(define (compile-halt inst)
  `(begin (movq 0 rdi)
	  (callq ,(global-symbol 'exit))))

(define (register? operand)
  (and (pair? operand) (eq? (car operand) 'reg)))

(define (immediate? operand)
  (number? operand))

(define (immediate-value imm)
  (+ (* 2 imm) 1))

(define (register-index reg)
  (cadr reg))

(define (global? exp)
  (global-symbol? exp))

(define (label? exp)
  (identifier? exp))

(define *machine-registers* #(rdi rsi rdx rcx r8 r9 rbx r12 r13 r14 r15 r10 r11))
(define (get-machine-register index)
  (vector-ref *machine-registers* index))
