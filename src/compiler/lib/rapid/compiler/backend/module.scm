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
    ((module ,declaration* ...)
     (receive (procedures data variables)
	 (let loop ((declaration* declaration*))
	   (if (null? declaration*)
	       (values '() '() '())
	       (receive (procedures data variables)
		   (loop (cdr declaration*))
		 (match (car declaration*)
		   ((procedure ,name ,code ...)
		    (values (cons (list name code) procedures)
			    data
			    variables))
		   ((datum ,name ,bytes)
		    (values procedures
			    (cons (list name (if (string? bytes)
						 (bytevector-append
						  (string->utf8 bytes)
						  #u8(0))
						 bytes))
				  data)
			    variables))
		   ((variable ,name ,init)
		    (values procedures
			    data
			    (cons (list name init) variables)))
		   (,_ (error "invalid module declaration" (car declaration*)))))))
       (compile-module procedures data variables)))	  
    (,_ (error "invalid module" module))))

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
	    (link-label (make-synthetic-identifier 'link))
	    (stmts (cadr procedure)))
	`(begin (align 8)
		,link-label
		(quad (- (+ ,start-label 2) ,link-label)) ;; 2 = VALUE_TAG_LINK
		,label
		,(compile-statements stmts label))))

    (define (compile-datum datum)
      (let ((label (car datum))
	    (link-label (make-synthetic-identifier 'link))
	    (bytes (cadr datum)))
	`(begin (align 8)
		,link-label
		(quad (- (+ ,start-label 2) ,link-label)) ;; 2 = VALUE_TAG_LINK
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
	  (vars-assembly (compile-vars vars))
	  (link-label (make-synthetic-identifier 'link)))
      (let-values (((code offsets)
		    (assemble `(begin (align 16)
				      ,start-label
				      (quad (- ,end-label ,start-label))
				      (quad ,(* 8 (length vars)))
				      ,procedures-assembly
				      ,datums-assembly
				      (align 8)
				      ,link-label
				      (quad (- (+ ,start-label 2) ,link-label))
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

(define (local-symbol symbol base)
  (let ((index (local-symbol-index symbol)))
    `(fs ,(* index 8) ,base))) 

(define (label? stmt)
  (identifier? stmt))

(define (compile-statements stmts start-label)
  `(begin ,@(map (lambda (stmt) (compile-statement stmt start-label)) stmts)))

(define (compile-statement stmt start-label)
  (match stmt
    (,label (guard (label? label)) (compile-label label))
    ((alloc ,num ,size ,reg* ...) (compile-alloc num size (map get-machine-register reg*)
						 start-label))
    ((halt) (compile-halt))
    ((jump ,reg) (guard (register? reg)) (compile-jump/reg (get-machine-register reg)))
    ((jump (,label)) (compile-jump/indirect label))
    ((jump ,label) (compile-jump/label label))
    ((record ,field* ... ,reg) (compile-record field* (get-machine-register reg)))
    ((load (,index ,record) ,reg) (compile-load index record (get-machine-register reg)))
    ((lea (,index ,record) ,reg) (compile-lea index record (get-machine-register reg)))
    ((store (,index ,record) ,reg) (compile-store record (get-machine-register reg) index))    
    ((call ,global-name ,reg* ...) (compile-call global-name (map get-machine-register reg*)))
    ((move ,operand ,reg) (compile-lea 0 operand (get-machine-register reg)))
    ((xchg ,reg* ...) (compile-xchg (map get-machine-register reg*)))
    ((add ,operand1 ,operand2 ,reg) (compile-add operand1 operand2 (get-machine-register reg)))
    ((branch ,input1 ,input2 ,clause* ...) (compile-branch input1 input2 clause*))
    ((global-fetch ,global ,reg) (compile-global-fetch global (get-machine-register reg)))
    ((dump ,filename ,entry ,reg* ...) (compile-dump filename entry
						     (map get-machine-register reg*)
						     start-label))
    (,_ (error "invalid statement" stmt))))

(define (compile-alloc num size live-registers start-label)
  ;; TODO: Handle large allocations larger than stack size
  (let ((ok-label (make-synthetic-identifier 'ok))
	(resume-label (make-synthetic-identifier 'resume))
	(bytes (* 8 (+ (* 2 num) size))))

    `(begin ,@(if (< bytes #x10000)
		  `((cmpw ,bytes sp)
		    (jae ,ok-label))
		  '())
	    (movq ,(global-symbol 'locals) ,(acc))
	    (movq ,(local-symbol 'heap-end (acc)) ,(acc))
	    (addq ,bytes ,(acc))
	    (cmpq ,(acc) rsp)
	    (jae ,ok-label)
	    ,@(if (even? (length live-registers))
		  '((addq 8 rsp))
		  '())
	    ,@(map (lambda (register)
		     `(pushq ,register))
		   live-registers)
	    (leaq (,start-label rip) ,(acc))
	    (pushq ,(acc))
	    (movq rsp rdi)
	    (movq ,(+ (length live-registers) 1) rsi)
	    (leaq (,resume-label rip) rbx)
	    (jmpq ,(global-symbol 'rapid-gc-wrapper))
	    ,resume-label
	    ,@(map (lambda (register)
		     `(popq ,register))
		   (reverse live-registers))
	    ;; Set stack pointer at the beginning of the short-term heap
	    (movq ,(global-symbol 'locals) ,(acc))
	    (movq ,(local-symbol 'heap-start (acc)) rsp)
	    ,ok-label)))

(define (compile-dump filename entry live-registers start-label)
  (let ((resume-label (make-synthetic-identifier 'resume)))
    `(begin ,@(if (even? (length live-registers))
		  '((addq 8 rsp))
		  '())
	    ,@(map (lambda (register)
		     `(pushq ,register))
		   live-registers)
	    (leaq (,start-label rip) ,(acc))
	    (pushq ,(acc))
	    ,(compile-operand filename (acc))
	    (pushq ,(acc))
	    ,(compile-operand entry 'rcx)
	    (popq rdx)
	    (movq rsp rdi) ;; we can move this to gc-dump-wrapper
	    (movq ,(+ (length live-registers) 1) rsi)
	    (leaq (,resume-label rip) rbx)
	    (jmpq ,(global-symbol 'rapid-gc-dump-wrapper))
	    ,resume-label
	    ,@(map (lambda (register)
		     `(popq ,register))
		   (reverse live-registers))
	    (movq ,(global-symbol 'locals) ,(acc))
	    (movq ,(local-symbol 'heap-start (acc)) rsp))))

(define (compile-global-fetch global register)
  `(movq ,(global-symbol global) ,register))

(define (compile-jump/reg register)
  `(jmpq ,register))

(define (compile-jump/indirect label)
  `(jmpq (,label rip)))

(define (compile-jump/label label)
  `(jmp ,label))

(define (compile-record fields register)
  (let ((record-length (length fields)))
    `(begin ,@(if (even? record-length)
		  `((subq 8 rsp))
		  '())
	    ,@(map compile-record-field (reverse fields))
	    (movq rsp ,register)
	    (pushq ,(+ (* 8 (+ 1 record-length)) 4))))) ; 4 = VALUE_TAG_RECORD

(define (compile-record-field field)
  ;; TODO: HAndle integer field
  (match field
    ((,base ,index* ... ,offset)
     `(begin
	,(compile-operand base (acc))
	,@(map (lambda (index)
		 `(movq (,(* 8 index) ,(acc)) ,(acc)))
	       index*)
	(addq ,offset ,(acc))
	(pushq ,(acc))))
    (,value (guard (integer? value)) `(pushq ,(immediate-value value)))
    (,_ (error "invalid record field" field))))	             

(define (compile-operand operand register)
  (cond
   ((integer? operand)
    `(movq ,(immediate-value operand) ,register))
   ((get-machine-register operand)
    => (lambda (source-register)
	 `(movq ,source-register ,register)))
   (else
    (match operand
      ((,label)
       `(movq (,label rip) ,register))
      ((,index ,label)
       `(movq ((+ ,label ,(* 8 index)) rip) ,register))
      (,operand
       `(leaq (,operand rip) ,register))))))

(define (compile-lea index record register)
  (if (zero? index)
      (compile-operand record register)
      `(begin ,(compile-operand record (acc))
	      (leaq (,(* 8 index) ,(acc)) ,register))))

(define (compile-xchg registers)
  `(begin
     (cond
      ((null? (cdr registers))
       '())
      ((null? (cddr registers))
       `((xchgq (car registers) ,(cadr registers))))
      (else
       (let ((registers (reverse (cons ,(acc) registers))))
	 `((movq ,(car registers) ,(acc))
	   ,@(map (lambda (source target)
		    (movq ,source ,target))
		  (cdr registers) registers)))))))

(define (compile-load index record register)
  (cond
   ((get-machine-register record)
    => (lambda (source-register)
	 (if (integer? index)
	     `(movq (,(* 8 index) ,source-register) ,register)
	     (let ((index-register (get-machine-register index)))
	       `(movq (,source-register ,index-register 8) ,register)))))
   ((label? record)
    (if (integer? index)
	`(movq ((+ ,record ,(* 8 index)) rip) ,register)
	(let ((index-register (get-machine-register index)))
	  `(begin (leaq (,record rip) ,(acc))
		  (movq (,(acc) ,index-register 8) ,register)))))
   (else
    (error "cannot point to a record" record))))

(define (compile-store record register index)
  (cond
   ((get-machine-register record)
    => (lambda (source-register)
	 (if (integer? index)
	     `(movq ,register (,(* 8 index) ,source-register))
	     (let ((index-register (get-machine-register index)))
	       `(movq ,register (,source-register ,index-register 8))))))
   ((label? record)
    (if (integer? index)
	`(movq ,register ((+ ,record ,(* 8 index)) rip))
	(let ((index-register (get-machine-register index)))
	  `(begin (leaq (,record rip) ,(acc))
		  (movq ,register (,(acc) ,index-register 8))))))
   (else
    (error "cannot point to a record" record))))

(define (compile-add operand1 operand2 register)
  (if (integer? operand1)
      (if (integer? operand2)
	  `(movq ,(immediate-value (+ operand1 operand2)) ,register)
	  `(leaq (,(- (immediate-value operand1) 1) ,(get-machine-register operand2))
		 ,register))
      (if (integer? operand2)
	  (compile-add operand2 operand1 register)
	  `(leaq (-1 ,(get-machine-register operand1)
		     ,(get-machine-register operand2))
		 ,register))))
      
(define (compile-branch input1 input2 clause*)
  `(begin
     ,(compile-operand input2 (acc))
     ,(if (integer? input1)
	  `(cmpq ,(acc) ,(immediate-value input1))
	  `(cmpq ,(acc) ,(get-machine-register input1)))
     ,@(map compile-branch-clause clause*)))

(define (compile-branch-clause clause)
  (match clause
    ((= ,label)
     `(je ,label))))

(define (compile-label label)
  label)

(define (compile-call callee . registers)
  ;; TODO: push and pop registers that are caller-save registers
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

;(define *machine-registers* #(rdi rsi rdx rcx r8 r9 rbx r12 r13 r14 r15 r10 r11))
;(define (get-machine-register index)
;  (vector-ref *machine-registers* index))

(define (register? obj)
  (and (get-machine-register obj) #t))

(define (acc) 'rax)

(define *machine-registers*
  '((r0 rdi)
    (r1 rsi)
    (r2 rdx)
    (r3 rcx)
    (r4 r8)
    (r5 r9)
    (r6 r10)
    (r7 r11)
    (v0 rbx)
    (v1 r12)
    (v2 r13)
    (v3 r14)
    (v4 r15)))
	
(define (get-machine-register register)
  (cond
   ((assq register *machine-registers*) => cadr)
   (else #f)))

(define (get-caller-save-registers)
  #(r0 r1 r2 r3 r4 r5 r6 r7))

(define (get-callee-save-registers)
  #(v0 v1 v2 v3 v4))
