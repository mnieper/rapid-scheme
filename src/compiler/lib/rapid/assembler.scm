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
				(-
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

(define (assembler-label assembler)
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

(define (rex-prefix w r x b)
  (+ #x40 (* 8 w) (* 4 r) (* 2 x) b))

(define *registers* '())
(define (make-register name type value rex)
  (list name type value rex))
(define (register-type register) (list-ref register 1))
(define (register-value register) (list-ref register 2))
(define (register-rex register) (list-ref register 3))
(define-syntax define-register
  (syntax-rules ()
    ((define-register name type value rex)
     (set! *registers*
	   (cons (make-register 'name 'type value rex)
		 *registers*)))))

(define (instruction-mnemonic instruction)
  (car instruction))
(define (instruction-operands instruction)
  (map make-operand (cdr instruction)))
(define (make-operand source)
  (cond
   ((assq source *registers*)
    => make-register-operand)
   ((immediate? source)
    (%make-operand 'imm source #f #f #f #f))
   (else
    (error "unknown operand type" source))))
(define (immediate? source)
  (or (label? source)
      (number? source)))

(define (make-register-operand register)
  (let ((type (register-type register))
	(value (register-value register))
	(rex (register-rex register)))
    (%make-operand type
		   (remainder value 8)
		   (if (>= value 8)
		       1
		       rex)
		   #f
		   #f
		   #f)))

(define-record-type <operand>
  (%make-operand type value rex.w rex.r rex.x rex.b)
  operand?
  (type operand-type)
  (value operand-value)
  (rex.w operand-rex.w)
  (rex.r operand-rex.r)
  (rex.x operand-rex.x)
  (rex.b operand-rex.b))

(define *instructions* '())
(define (make-instruction mnemonic operand-types opcode)
  (list (list mnemonic operand-types) opcode))
(define-syntax define-instruction
  (syntax-rules ()
    ((define-instruction (mnemonic operand ...) . opcode)
     (set! *instructions* (cons (cons (list 'mnemonic '(operand ...))
				      (make-instruction 'opcode))
				*instructions*)))))
(define (get-instruction mnemonic operands)
  (let ((types (map operand-type operands)))
    (cond
     ((assoc (list mnemonic types) *instructions*)
      => cdr)
     (else
      (error "invalid instruction" mnemonic operands)))))
(define-record-type <instruction>
  (make-instruction opcode)
  instruction?
  (opcode instruction-opcode))

(define (assemble inst) (assembler-assemble (current-assembler) inst))

(define (assembler-assemble assembler inst)

  (define patch #f)

  (define (relative! size)
    (set! patch (vector (assembler-location assembler) size)))
  
  (define (emit bytevector)
    (write-bytevector bytevector (assembler-port assembler))
    (assembler-set-location! assembler
			     (+ (assembler-location assembler)
				(bytevector-length bytevector))))

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
  
  (define (write-rex-prefix operands)
    (let ((prefix (get-rex-prefix operands)))
      (when prefix
	(emit-byte prefix))))

  (define (get-reg-value operands)
    (let loop ((operands operands))
      (case (operand-type (car operands))
	((reg8 reg16 reg32 reg64)
	 (operand-value (car operands)))
	(else
	 (loop (cdr operands))))))

  (define (get-imm-value operands)
    (let loop ((operands operands))
      (case (operand-type (car operands))
	((imm)
	 (operand-value (car operands)))
	(else
	 (loop (cdr operands))))))
  
  (define (get-rex-prefix operands)
    (let ((rex.w (join-bits (map operand-rex.w operands)))
	  (rex.r (join-bits (map operand-rex.r operands)))
	  (rex.x (join-bits (map operand-rex.x operands)))
	  (rex.b (join-bits (map operand-rex.b operands))))
      (and (or rex.w rex.r rex.x rex.b)
	   (rex-prefix (or rex.w 0)
		       (or rex.r 0)
		       (or rex.x 0)
		       (or rex.b 0)))))

  (define (join-bits bits)
    (let loop ((join #f) (bits bits))
      (if (null? bits)
	  #f
	  (let ((bit (car bits))
		(join (join-bits (cdr bits))))
	    (if bit
		(begin
		  (unless (= bit join)
		    (error "invalid operand combination"))
		  bit)
		join)))))

  (let ((mnemonic (instruction-mnemonic inst))
	(operands (instruction-operands inst)))
    (let ((instruction (get-instruction mnemonic operands)))
      (write-rex-prefix operands)
      (let loop ((opcode (instruction-opcode instruction)))
	(unless (null? opcode)
	  (let* ((component (car opcode))
		 (opcode (cdr opcode)))
	    (case component
	      ((iw)
	       (emit-word (get-imm-value operands)))
	      ((id)
	       (emit-long (get-imm-value operands)))
	      ((cd)
	       (relative! 4)
	       (emit-long (get-imm-value operands)))
	      (else
	       (if (and (pair? opcode)
			(memq (car opcode) '(+rb +rw +rd +rq)))
		   (begin
		     (emit-byte (+ component
				   (get-reg-value operands)))
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
