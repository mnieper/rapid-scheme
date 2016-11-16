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
  (let ((start-label (make-synthetic-identifier 'module-start))
	(end-label (make-synthetic-identifier 'module-end)))
    
    (define (compile-procedures procedures)
      `(begin ,@(map compile-procedure procedures)))

    (define (compile-datums datums)
      `(begin ,@(map compile-datum datums)))

    (define (compile-vars vars)
      ;; TODO: Add some link for GC. 
      `(begin ,@(map compile-var vars)))

    (define (compile-procedure procedure)
      (let ((label (car procedure))
	    (stmts (cadr procedure)))
	`(begin (align 8)
		(quad (- ,label ,start-label)) ;; TODO: GC-FLAG
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
		(quad ,(immediate-value init)))))

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

(define (compile-statements stmts)
  `(begin ,@(map compile-statement stmts)))

(define (compile-statement stmt)
  (match stmt
    (,label (guard (label? label)) (compile-label label))
    ((call ,global-name) (compile-call global-name))
    ((assign ,register-name (op ,operation) ,input* ...)
     (compile-operation register-name operation input*))
    ((assign ,register-name ,input) (compile-assignment register-name input))
    ((compare ,input1 ,input2) (compile-comparison input1 input2))
    ((branch (cond ,condition) (label ,label)) (compile-branch condition label))
    ((set! ,base-name ,offset ,value) (compile-set! base-name offset value))
    ((goto ,target) (compile-jump target))
    ((halt) (compile-halt))
    (,_ (error "invalid statement" stmt))))

;; Module language
;;
;; <input> : (reg <register-name>)
;;         | (const <constant-value>)
;;         | (global <global-name>)
;;         | (ref <register-name> <offset>)
;;         | (byte-ref <register-name> <offset>)
;;         | (local <local-name>)
;; <offset>: (reg <register-name>) | (const <constant-value>)
;;
;; (assign <register-name> <input>)
;; (assign <register-name> (op <operation-name>) <input> ...)
;; (perform (op <operation-name>) <input> ...)
;; (compare <input> <input>)
;; (branch (cond <) (label <label>))
;; (goto (label <label>))
;; (goto (reg <register-name>))
;; (goto (ref <register-name> <offset>))
;; (call <global-name>)
;; (set! <register name> <offset> <offset>)
;; (byte-set! <register name> <offset> <offset>)
;; (assign <register-name> (record ...))

(define (compile-jump target)
  (match target
    ((reg ,register-name)
     `(jmpq ,(get-machine-register register-name)))
    ((label ,label-name)
     `(jmp ,label-name))
    ((ref ,register-name ,offset)
     `(jmpq ,(compile-reference register-name offset)))
    (,_
     (error "invalid goto target" target))))

(define (compile-reference base-name offset)
  (let ((base-register (get-machine-register base-name)))
    (match offset
      ((const ,constant-value)
       `(,(* 8 constant-value) ,base-register))
      ((reg ,index-register-name)
       `(,base-register ,(get-machine-register index-register-name) 8)))))     

(define (compile-expression register exp)
  (match exp
    ((const ,constant-value)
     `(movq ,(immediate-value constant-value) ,register))
    ((reg ,source-register-name)
     `(movq ,(get-machine-register source-register-name) ,register))
    ((local ,local-name)
     `(leaq (,local-name rip) ,register))
    ((global ,global-name)
     `(movq ,(global-symbol global-name) ,register))
    ((ref ,register-name ,offset)
     `(movq ,(compile-reference register-name offset) ,register))
    (,_
     (error "invalid input" exp))))

(define (compile-set! base-name offset value)
  (let ((target (compile-reference base-name offset)))
    (match value
      ((reg ,register-name)
       `(movq ,(get-machine-register register-name) ,target))
      ((const ,constant-value)
       `(movq ,(immediate-value constant-value) ,target)))))

(define (compile-comparison input1 input2)
  `(begin ,(compile-expression (acc) input1)
	  ,(match input2
	    ((const ,constant-value)
	     `(cmpq ,(immediate-value constant-value) ,(acc)))     
	    ((reg ,source-register-name)
	     `(cmpq ,(get-machine-register source-register-name) ,(acc)))
	    ((ref ,register-name ,offset)
	     `(cmpq ,(compile-reference register-name offset) ,(acc)))
	    (,_ (error "bad comparison operand" input2)))))

(define (compile-branch condition label)
  (let ((mnemonic (cadr (assq condition '((= je))))))
    `(,mnemonic ,label)))

(define (compile-assignment register-name input)
  (let ((register (get-machine-register register-name)))
    (compile-expression register input)))
    
(define (compile-operation register-name operation input*)  
  (let ((register (get-machine-register register-name)))
    (case operation
      ((+) (compile-addition register input*))
      (else
       (error "unknown operation" operation)))))
  
(define (compile-addition target input*)
  (let ((input1 (car input*))
	(input2 (cadr input*)))
    `(begin ,(compile-expression (acc) input1)
	    ,(compile-expression target input2)
	    (leaq (-1 ,(acc) ,target) ,target))))

(define (compile-label label)
  label)

(define (compile-call callee)
  (case callee
    ((exit)
     `(begin (sarq rdi)
	     (callq ,(global-symbol 'exit))))
    ((fputs)
     `(callq ,(global-symbol 'fputs)))
    (else
     (error "unknown global procedure" callee))))

(define (compile-halt)
  `(begin (movq 0 rdi)
	  (callq ,(global-symbol 'exit))))

(define (immediate-value imm)
  (+ (* 2 imm) 1))

(define (label? exp)
  (identifier? exp))

(define *machine-registers* #(rdi rsi rdx rcx r8 r9 rbx r12 r13 r14 r15 r10 r11))
(define (get-machine-register index)
  (vector-ref *machine-registers* index))
(define (acc) 'rax)
