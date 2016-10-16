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

;;; Syntax definitions

(define-syntax asm:
  (syntax-rules ()
    ((asm: . instruction)
     (assemble `instruction))))

(define-syntax define-register
  (syntax-rules ()
    ((define-register name types value rex)
     (set! *registers*
	   (cons (cons 'name (make-register 'types value rex))
		 *registers*)))))

(define-syntax define-instruction
  (syntax-rules ()
    ((define-instruction (mnemonic operand ...) . opcode)
     (set! *instructions* (cons (make-instruction 'mnemonic '(operand ...) 'opcode)
				*instructions*)))))

;;; Registers

(define-record-type <register>
  (make-register types value rex)
  %register?
  (types register-types)
  (value register-value)
  (rex register-rex))

(define *registers* '())

;;; Instructions

(define-record-type <instruction>
  (make-instruction mnemonic operand-types opcode)
  instruction?
  (mnemonic instruction-mnemonic)
  (operand-types instruction-operand-types)
  (opcode instruction-opcode))

(define (get-instruction mnemonic operands)
  (let loop ((instructions *instructions*))
    (when (null? instructions)
      (error "invalid instruction" mnemonic operands))
    (let ((continue (lambda () (loop (cdr instructions))))
	  (instruction (car instructions)))
      (define types (instruction-operand-types instruction))
      (if (and (eq? (instruction-mnemonic instruction) mnemonic)
	       (= (length types) (length operands)))	       
	  (let loop ((operands operands)
		     (types (instruction-operand-types instruction)))
	    (if (null? operands)
		instruction
		(if (operand-matches (car operands) (car types))
		    (loop (cdr operands) (cdr types))
		    (continue))))
	  (continue)))))
      
(define *instructions* '())

;;; Operands

(define-record-type <operand>
  (%make-operand matcher processor)
  operand?
  (matcher operand-matcher)
  (processor operand-processor))

(define (operand-matches operand type)
  ((operand-matcher operand) type))

(define (operand-process! operand code type)
  ((operand-processor operand) code type))

(define (make-operand source)
  (cond
   ((register? source) (make-register-operand source))
   ((immediate? source) (make-immediate-operand source))
   #;((memory? source) (make-memory-operand source))
   #;((memory-absolute? source) (make-memory-absolute-operand source))
   (else
    (error "invalid operand" source))))

(define (register? source)
  (assq source *registers*))

(define (immediate? source)
  (or (number? source)
      (label? source)))

(define (memory? source)
  (and (pair? source)
       (or (register? (car source))
	   (immediate? (car source)))))

(define (memory-absolute? source)
  (and (pair? source)
       (memory? (car source))))

(define (make-immediate-operand source)
  (%make-operand
   (lambda (type)
     (case type
       ((imm8 imm16 imm32 imm64 imm16/32 imm32/64) #t)
       (else #f)))
   (lambda (code type)
     (code-set-imm! code source))))

(define (make-register-operand source)
  (let ((register (cdr (assq source *registers*))))
    (%make-operand
     (lambda (type)
       (and (memq type (register-types register)) #t))
     (lambda (code type)
       (case type
	 ((reg8 reg16 reg16/32 reg32 reg64)
	  (code-set-reg! code (register-value register))
	  (code-set-rex.r! code (register-rex register))
	  )
	 (else
	  (error "unsupported operand type" type)))))))

;;; Code to be assembled

(define-record-type <code>
  (%make-code rex.w rex.r rex.x rex.b)
  code?
  (disp code-disp code-set-disp!)
  (imm code-imm code-set-imm!)
  (reg code-reg code-set-reg!)
  (rex.w code-rex.w code-set-rex.w!)
  (rex.r code-rex.r code-set-rex.r!)
  (rex.x code-rex.x code-set-rex.x!)
  (rex.b code-rex.b code-set-rex.b!)
  ;; add patch information from below
  )

(define (make-code)
  (%make-code #f #f #f #f))

;;; Assembler object

(define-record-type <assembler>
  (%make-assembler port location labels patches)
  assembler?
  (port assembler-port)
  (location assembler-location assembler-set-location!)
  (labels assembler-labels assembler-set-labels!)
  (patches assembler-patches assembler-set-patches!))

(define (make-assembler)
  (%make-assembler (open-output-bytevector) 0 '() '()))

(define (assembler-add-label! assembler label)
  (assembler-set-labels! assembler
			 (cons label (assembler-labels assembler))))

(define (for-each-label proc assembler)
  (for-each proc (assembler-labels assembler)))

(define (assembler-patch-code! assembler location value size)
  (assembler-set-patches! assembler
			  (cons (make-patch location value size)
				(assembler-patches assembler))))

(define-record-type <patch>
  (make-patch location value size)
  patch?
  (location patch-location)
  (value patch-value)
  (size patch-size))
  
(define (for-each-patch proc assembler)
  (for-each proc (assembler-patches assembler)))

(define (assembler-get-code assembler)
  (let ((code (get-output-bytevector (assembler-port assembler))))
    (for-each-patch
     (lambda (patch)
       (bytevector-integer-set! code
				(patch-location patch)
				(+
				 (bytevector-integer-ref code
							 (patch-location patch)
							 (patch-size patch))
				 (patch-value patch))
				(patch-size patch)))
     assembler)
    (for-each-label
     (lambda (label)
       (let ((label-location (label-location label)))
	 (for-each-use
	  (lambda (use)
	    (bytevector-integer-set! code
				     (label-use-location use)
				     (+ label-location
				        (bytevector-integer-ref code
								(label-use-location use)
								(label-use-size use)))
				     (label-use-size use)))
	  label)))
     assembler)
    code))

;;; Assembler labels

(define-record-type <label>
  (make-label assembler location uses)
  label?
  (assembler label-assembler)
  (location label-location label-set-location!)
  (uses label-uses label-set-uses!))

(define-record-type <label-use>
  (make-label-use location size)
  label-use?
  (location label-use-location)
  (size label-use-size))

(define (assembler-make-label assembler)
  (make-label assembler (assembler-location assembler) '()))

(define (label-here! label)
  (let ((assembler (label-assembler label)))
    (label-set-location! label (assembler-location assembler))
    (assembler-add-label! assembler label)))

(define (label-add-use! label size)
  (let ((assembler (label-assembler label))) 
    (label-set-uses! label
		     (cons (make-label-use (assembler-location assembler) size)
			   (label-uses label)))))

(define (for-each-use proc label)
  (for-each proc (label-uses label)))


;;; Instruction prefix definitions

(define (rex-prefix w r x b)
  (+ #x40 (* 8 (or w 0)) (* 4 (or r 0)) (* 2 (or x 0)) (or b 0)))
(define prefix/lock #xF0)
(define prefix/repne #xF2)
(define prefix/repnz #xF2)
(define prefix/rep #xF3)
(define prefix/repe #xF3)
(define prefix/repz #xF3)
(define prefix/fs #x64)
(define prefix/gs #x65)
(define prefix/operand-size #x66)
(define prefix/address-size #x67)

;;; Assembler

(define (assemble inst) (assembler-assemble (current-assembler) inst))


(define (align integer alignment)
  (let*
      ((alignment (if (zero? alignment)
		      1
		      alignment))
       (integer (+ integer alignment -1)))
    (- integer (remainder integer alignment))))

(define (assembler-align! assembler alignment)
  (assembler-emit assembler
		  (make-bytevector (- (align (assembler-location assembler) alignment)
				      (assembler-location assembler)))))

(define (assembler-emit assembler bytevector)
  (write-bytevector bytevector (assembler-port assembler))
  (assembler-set-location! assembler
			   (+ (assembler-location assembler)
			      (bytevector-length bytevector))))

(define (assembler-assemble assembler inst)
  
  (define patch #f)

  (define (relative! size)
    (set! patch (vector (- (assembler-location assembler)) size)))
  
  (define code (make-code))

  (define (emit bytevector)
    (assembler-emit assembler bytevector))

  (define (emit-value value size)
    (if (label? value)
	(begin
	  (label-add-use! value size)
	  (emit (make-bytevector size 0)))
	(begin
	  (emit (integer->bytevector value size)))))
  
  (define (emit-byte int)
    (emit-value int 1))

  (define (emit-word int)
    (emit-value int 2))

  (define (emit-long int)
    (emit-value int 4))

  (define (emit-quad int)
    (emit-value int 8))

  (define (get-rex-prefix)
    (let ((rex.w (code-rex.w code))
	  (rex.r (code-rex.r code))
	  (rex.x (code-rex.x code))
	  (rex.b (code-rex.b code)))
      (and (or rex.w rex.r rex.x rex.b)
	   (rex-prefix rex.w rex.r rex.x rex.b))))
  
  (define (write-rex-prefix)
    (let ((prefix (get-rex-prefix)))
      (when prefix
	(emit-byte prefix))))
    
  (let ((mnemonic (car inst))
	(operands (map make-operand (cdr inst))))
    (let ((instruction (get-instruction mnemonic operands)))
      (define opcode (instruction-opcode instruction))
      
      (for-each
       (lambda (operand type)
	 (operand-process! operand code type))
       operands (instruction-operand-types instruction))

      (when (eq? 'rex (car opcode))
	(code-set-rex.w! code 1)
	(set! opcode (cdr opcode)))	
      (write-rex-prefix)

      (let loop ((opcode opcode))
	(unless (null? opcode)
	  (let* ((component (car opcode))
		 (opcode (cdr opcode)))
	    (case component
	      ((ib)
	       (emit-byte (code-imm code)))
	      ((iw)
	       (emit-byte (code-imm code)))
	      ((id)
	       (emit-byte (code-imm code)))
	      ((iq)
	       (emit-byte (code-imm code)))
	      ((cd)
	       (relative! 4)
	       (emit-long (code-imm code)))
	      (else
	       (if (and (pair? opcode)
			(memq (car opcode) '(+rb +rw +rd +rq)))
		   (begin
		     (emit-byte (+ component (code-reg code)))
		     (loop (cdr opcode)))
		   (begin
		     (emit-byte component)
		     (loop opcode))))))))

      (when patch
	(assembler-patch-code! assembler
			       (vector-ref patch 0)
			       (assembler-location assembler)
			       (vector-ref patch 1))))))
    
(define current-assembler (make-parameter #f))
