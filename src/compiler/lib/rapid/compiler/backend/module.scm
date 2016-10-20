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

(define-record-type <module-reference>
  (make-reference module offset)
  module-reference?
  (module module-reference-module)
  (offset module-reference-offset))

(define-record-type <module>
  (%make-module offsets code)
  module?
  (offsets module-offsets)
  (code module-code))

(define (make-module procedures datums vars)
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
		,label
		(quad (- ,label ,start-label)) ;; GC-FLAG
		,(bytevector->assembly bytes))))

    (define (compile-var var)
      (let ((label (car var))
	    (init (cadr var)))
	`(begin (align 8)
		,label
		(quad ,init))))
    
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

(define (label-offset module label)
  (imap-ref (module-offsets module) label))

(define (module-reference module label)
  (make-reference module (label-offset module label)))

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

(define (compile-instruction inst)
  (case (instruction-name inst)
    ((exit)
     (compile-exit inst))
    ((halt)
     (compile-halt inst))
    (else (error "invalid instruction" inst))))

(define (compile-exit inst)
  (let ((operand (car (instruction-operands inst))))	 
    `(begin ,(load 'rdi operand)
	    (sarq rdi)
	    (callq ,(global-symbol 'exit)))))

(define (compile-halt inst)
  `(begin (movq 0 rdi)
	  (callq ,(global-symbol 'exit))))

(define (load reg operand)
  (cond
   ((register? operand)
    (load-register reg operand))
   ((immediate? operand)
    (load-immediate reg operand))
   (else
    (error "invalid operand" operand))))

(define (register? operand)
  (and (pair? operand) (eq? (car operand) 'reg)))

(define (immediate? operand)
  (number? operand))

(define (immediate-value imm)
  (+ (* 2 imm) 1))

(define (register-index reg)
  (cadr reg))

(define (load-register target reg)
  (let ((index (register-index reg)))
    (let ((source (get-machine-register index)))
      (if (eq? target source)
	  '(begin)
	  `(movq ,source ,target)))))

(define (load-immediate target imm)
  (let ((value (immediate-value imm)))
    `(movq ,value ,target)))

(define *machine-registers* #(rbx r12 r13 r14 r15))
(define (get-machine-register index)
  (vector-ref *machine-registers* index))
