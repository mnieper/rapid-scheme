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

(define (get-register name)
  (cond
   ((assq name *registers*)
    => cdr)
   (else
    (error "unknown register" name))))

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

;;; Instruction components

(define (modrm-byte mod reg r/m)
  (+ (* 64 mod) (* 8 reg) r/m))

(define (sib-byte scale index base)
  (+ (* 64 scale) (* 8 index) base))

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
   ((memory? source) (make-memory-operand source))
   (else
    (error "invalid operand" source))))

(define (register? source)
  (assq source *registers*))

(define (immediate? source)
  (or (number? source)
      (identifier? source)
      (label-expression? source)))

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
  (let ((register (get-register source)))
    (%make-operand
     (lambda (type)
       (and (memq type (register-types register)) #t))
     (lambda (code type)
       (case type
	 ((reg8 reg16 reg16/32 reg32 reg64)
	  (code-set-reg! code (register-value register))
	  (code-set-rex.r! code (register-rex register)))
	 (else
	  (error "unsupported operand type" type)))))))

(define (make-memory-operand source)
  (let-values (((base index scale disp) (get-sib+disp source)))
    (%make-operand
     (lambda (type)
       (case type
	 ((reg/mem64) #t)
	 (else #f)))
     (lambda (code type)
       (case type
	 ((reg/mem64)
	  (code-set-disp! code disp)
	  (code-set-base! code base)
	  (code-set-index! code index)
	  (code-set-scale! code scale)
	  (when base
	    (code-set-rex.b! code (register-rex base)))
	  (when index
	    (code-set-rex.x! code (register-rex index)))))))))

(define (get-sib+disp source)
  (let-values
      (((disp source)
	(if (register? (car source))
	    (values #f source)
	    (values (car source) (cdr source)))))
    (case (length source)
      ((0) (values #f #f 0 disp))
      ((1) (values (get-register (list-ref source 0))
		   #f 0 disp))
      ((2) (values (get-register (list-ref source 0))
		   (get-register (list-ref source 1))
		   0 disp))
      ((3) (values (get-register (list-ref source 0))
		   (get-register (list-ref source 1))
		   (get-scale (list-ref source 2)) disp))
      (else
       (error "invalid memory operand" source)))))

(define (get-scale scale)
  (case scale
    ((#f 1) 0)
    ((2) 1)
    ((4) 2)
    ((8) 3)
    (else
     (error "invalid scale" scale))))

;;; Code to be assembled

(define-record-type <code>
  (%make-code rex.w rex.r rex.x rex.b)
  code?
  (disp code-disp code-set-disp!)
  (imm code-imm code-set-imm!)
  (reg code-reg code-set-reg!)
  (base code-base code-set-base!)
  (index code-index code-set-index!)
  (scale code-scale code-set-scale!)
  (rex.w code-rex.w code-set-rex.w!)
  (rex.r code-rex.r code-set-rex.r!)
  (rex.x code-rex.x code-set-rex.x!)
  (rex.b code-rex.b code-set-rex.b!))

(define (make-code)
  (%make-code #f #f #f #f))

;;; Patches
(define-record-type <patch>
  (make-patch position size expression)
  patch?
  (position patch-position)
  (size patch-size)
  (expression patch-expression))

;;; Assembler state

(define-record-type <assembler>
  (%make-assembler port position)
  code-vector?
  (port assembler-port)
  (position assembler-position assembler-set-position!))

(define (make-assembler)
  (%make-assembler (open-output-bytevector) 0))

(define (assembler-get-code assembler)
  (get-output-bytevector (assembler-port assembler)))

(define (assembler-emit assembler bytevector)
  (write-bytevector bytevector (assembler-port assembler))
  (assembler-set-position! assembler
			   (+ (assembler-position assembler)
			      (bytevector-length bytevector))))

(define (assembler-align! assembler alignment)
  (assembler-emit assembler
		  (make-bytevector (- (align (assembler-position assembler) alignment)
				      (assembler-position assembler)))))

(define (align integer alignment)
  (let*
      ((alignment (if (zero? alignment)
		      1
		      alignment))
       (integer (+ integer alignment -1)))
    (- integer (remainder integer alignment))))
  
(define (assemble text)
  (let ((assembler (make-assembler)))
    (let loop ((text text)
	       (offsets (make-imap eq?))
	       (patches '()))
      (if (null? text)
	  (let ((code (assembler-get-code assembler)))
	    (patch-code! code offsets patches)
	    (values code offsets))
	  (let-values (((offsets patches)
			(assemble-statement assembler
					    (car text)
					    offsets
					    patches)))
	    (loop (cdr text) offsets patches))))))

(define (patch-code! code offsets patches)
  (for-each (lambda (patch)
	      (apply-patch! code offsets patch))
	    patches))

(define (apply-patch! code offsets patch)
  (let ((position (patch-position patch))
	(size (patch-size patch))
	(expression (patch-expression patch)))
    (let ((value
	   (cond
	    ((label-difference? expression)
	      (- (imap-ref offsets (label-difference-minuend expression))
		 (imap-ref offsets (label-difference-subtrahend expression))))
	     (else
	      (error "invalid assembler expression" expression)))))
      (bytevector-integer-set! code
			       position
			       (+ (bytevector-integer-ref code position size)
				  value)
			       size))))

(define (make-label-difference minuend subtrahend)
  `(- ,minuend ,subtrahend))

(define (label-difference? exp)
  (and (pair? exp)
       (eq? (car exp) '-)))

(define (label-difference-minuend exp)
  (cadr exp))

(define (label-difference-subtrahend exp)
  (caddr exp))

(define (label-expression? exp)
  (or (label-difference? exp)))

(define (assemble-statement assembler stmt offsets patches)
  (cond
   ;; TODO: Add sequence stmt
   ((identifier? stmt)
    (assemble-label assembler stmt offsets patches))
   ((align-directive? stmt)
    (assemble-align-directive assembler stmt offsets patches))
   ((assembler-instruction? stmt)
    (assemble-instruction assembler stmt offsets patches))
   (else
    (error "invalid assembler statement" stmt))))

(define (align-directive? stmt)
  (and (pair? stmt) (eq? (car stmt) 'align)))

(define (align-directive-alignment stmt)
  (cadr stmt))

(define (assembler-instruction? stmt)
  (pair? stmt))

(define (assemble-label assembler label offsets patches)
  (let ((offsets (imap-replace offsets label (assembler-position assembler))))
    (values offsets patches)))
  
(define (assemble-align-directive assembler directive offsets patches)
  (let ((alignment (align-directive-alignment directive)))
    (assembler-align! assembler alignment)
    (values offsets patches)))

(define (assemble-instruction assembler inst offsets patches)
  (define code (make-code))

  (define after-label (make-synthetic-identifier 'after-inst))

  (define (relative! size)
    (let ((patch-label (make-synthetic-identifier 'patch)))    
      (let ((patch (make-patch (assembler-position assembler)
			       size
			       (make-label-difference after-label patch-label))))
	(set! patches (add-patch patches patch)))))
			     
  (define (emit bytevector)
    (assembler-emit assembler bytevector))

  (define (emit-value value size)
    (if (label-expression? value)
	(let ((patch (make-patch (assembler-position assembler)
				 size
				 value)))
	  (set! patches (add-patch patches patch))
	  (emit (make-bytevector size 0)))
	(emit (integer->bytevector value size))))

  (define (emit-byte int)
    (emit-value int 1))

  (define (emit-word int)
    (emit-value int 2))

  (define (emit-long int)
    (emit-value int 4))

  (define (emit-quad int)
    (emit-value int 8))

  (define (emit-modrm-sib-disp)
    (emit-byte (modrm-byte (if (code-base code)
			       #b10
			       #b00)
			   (code-reg code) #b100))
    (emit-byte (sib-byte (code-scale code)
			 (if (code-index code)
			     (register-value (code-index code))
			     #b100)
			 (if (code-base code)
			     (register-value (code-base code))
			     #b101)))
    (emit-long (or (code-disp code) 0)))
  
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
	       (emit-word (code-imm code)))
	      ((id)
	       (emit-double (code-imm code)))
	      ((iq)
	       (emit-quad (code-imm code)))
	      ((cd)
	       (relative! 4)
	       (emit-long (code-imm code)))
	      ((/)
	       (code-set-reg! code (car opcode))
	       ;; TODO: The following is in general not the most effective encoding.
	       (emit-modrm-sib-disp)
	       (loop (cdr opcode)))
	      (else
	       (if (and (pair? opcode)
			(memq (car opcode) '(+rb +rw +rd +rq)))
		   (begin
		     (emit-byte (+ component (code-reg code)))
		     (loop (cdr opcode)))
		   (begin
		     (emit-byte component)
		     (loop opcode))))))))

      (values offsets patches))))

(define (add-patch patches patch)
  (cons patch patches))
