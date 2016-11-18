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
		   ((data ,name ,bytes)
		    (values procedures
			    (cons (list name bytes) data)
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
    ((halt) (compile-halt))
    ((jump ,reg) (guard (register? reg)) (compile-jump/reg (get-machine-register reg)))
    ((jump ,label) (compile-jump/label label))
    ((record ,field* ... ,reg) (compile-record field* (get-machine-register reg)))
    ((load (,index ,record) ,reg) (compile-load index record (get-machine-register reg)))
    ((lea (,index ,record) ,reg) (compile-offset index record (get-machine-register reg)))
    ((store (,index ,record) ,reg) (compile-store record (get-machine-register reg) index))    
    ((call ,global-name) (compile-call global-name))
    ((move ,operand ,reg) (compile-offset 0 operand (get-machine-register reg)))
    ((add ,operand1 ,operand2 ,reg) (compile-add operand1 operand2 (get-machine-register reg)))
    ((branch ,input1 ,input2 ,clause* ...) (compile-branch input1 input2 clause*))
    ((global-fetch ,global ,reg) (compile-global-fetch global (get-machine-register reg)))
    (,_ (error "invalid statement" stmt))))

(define (compile-global-fetch global register)
  `(movq ,(global-symbol global) ,register))

(define (compile-jump/reg register)
  `(jmpq ,register))

(define (compile-jump/label label)
  `(jmp ,label))

(define (compile-record fields register)
  (let ((record-length (length fields)))
    `(begin ,@(if (even? record-length)
		  `((subq 8 rsp))
		  '())
	    ,@(map compile-record-field (reverse fields))
	    (movq rsp ,register)
	    (pushq ,record-length)))) ;; FIXME: Add mark for GC

(define (compile-record-field field)
  (match field
    ((,base ,index* ... ,offset)
     `(begin
	,(compile-operand base (acc))
	,@(map (lambda (index)
		 `(movq (,(* 8 index) ,(acc)) ,(acc)))
	       index*)
	(addq ,offset ,(acc))
	(pushq ,(acc))))
    (,_ (error "invalid record field" field))))	             

(define (compile-operand operand register)
  (cond
   ((integer? operand)
    `(movq ,(immediate-value operand) ,register))
   ((get-machine-register operand)
    => (lambda (source-register)
	 `(movq ,source-register ,register)))
   (else
    `(leaq (,operand rip) ,register))))

(define (compile-offset index record register)
  (if (zero? index)
      (compile-operand record register)
      `(begin ,(compile-operand record (acc))
	      (leaq (,(* 8 index) ,(acc)) ,register))))

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

