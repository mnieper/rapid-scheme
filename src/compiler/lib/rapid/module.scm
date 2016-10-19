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

(define-record-type <module-entry>
  (make-reference module)
  module-reference?
  (module module-reference-module reference-set-module!)
  (offset module-reference-offset reference-set-offset!))

(define-record-type <module-procedure>
  (make-procedure reference text)
  module-procedure?
  (reference module-procedure-reference)
  (text procedure-text procedure-set-text!))

(define (procedure-get-text procedure)
  (reverse (procedure-text procedure)))

(define-record-type <module-datum>
  (make-datum reference bytes)
  module-datum?
  (reference module-datum-reference)
  (bytes datum-bytes))

(define-record-type <module-var>
  (make-var reference init)
  module-var?
  (reference module-var-reference)
  (init var-init))

(define-record-type <module>
  (%make-module assembler datums vars code)
  module?
  (assembler module-assembler)
  (code module-code module-set-code!)
  (datums module-datums module-set-datums!)
  (procedures module-procedures module-set-procedures!)
  (vars module-vars module-set-vars!))

(define (make-module)
  (%make-module (make-assembler) '() '() #f))

(define (for-each-datum proc module)
  (for-each proc (reverse (module-datums module))))

(define (for-each-procedure proc module)
  (for-each proc (reverse (module-procedures module))))

(define (for-each-var proc module)  
 (for-each proc (reverse (module-vars module))))

(define (var-count module)
  (length (module-vars module)))

(define (module-add-datum module bytes)
  (let ((datum (make-datum (make-reference module) bytes)))
    (module-set-datums! module
			(cons datum (module-datums module)))
    datum))

(define (module-add-procedure module)
  (let ((procedure (make-procedure (make-reference module) '())))
    (module-set-procedures! module
			    (cons procedure (module-procedures module)))
    procedure))

(define (add-instruction! instruction)
  (let ((procedure (module-current-procedure)))
    (procedure-set-text! procedure (cons instruction (procedure-text procedure)))))

(define (module-add-var module init)
  (let ((var (make-var (make-reference module) init)))
    (module-set-vars! module
		      (cons var (module-vars module)))
    var))

(define (reference-here! module reference)
  (let ((label (assembler-make-label (module-assembler module))))
    (reference-set-offset! reference (label-location label))))

(define (module-get-code module)
  (let ((code (module-code module)))
    (or code
	(parameterize ((current-assembler (module-assembler module)))
	  (define start-label (assembler-make-label (current-assembler)))	  
	  (define end-label (assembler-make-label (current-assembler)))
	  (label-here! start-label)
	  (asm: quad 0)
	  (asm: quad ,(* 8 (var-count module)))

	  (for-each-datum
	   (lambda (datum)
	     (assembler-align! (current-assembler) 8)
	     (let ((label (assembler-make-label (current-assembler))))
	       ;; FIXME: HAVE TO ADD GC-MARK BIT TO DIFFERENCE
	       (asm: quad ,(- (label-location label)
			      (label-location start-label))))
	     (reference-here! module (module-datum-reference datum))
	     (do ((i 0 (+ i 1)))
		 ((= i (bytevector-length (datum-bytes datum))))
	       (asm: byte ,(bytevector-u8-ref (datum-bytes datum) i))))
	   module)

	  (for-each-procedure
	   (lambda (procedure)
	     (assembler-align! (current-assembler) 8)
	     (let ((label (assembler-make-label (current-assembler))))
	       ;; FIXME: HAVE TO ADD GC-MARK BIT TO DIFFERENCE
	       ;; TODO: Refactor this code and the corresponding code for datums
	       (asm: quad ,(- (label-location label)
			      (label-location start-label))))
	     (reference-here! module (module-procedure-reference procedure))
	     (for-each
	      (lambda (instruction)
		(asm: ,@instruction))
	      (procedure-get-text procedure)))
	   module)
	  
	  (assembler-align! (current-assembler) 8)
	  (for-each-var
	   (lambda (var)
	     (reference-here! module (module-var-reference var))
	     (asm: quad ,(var-init var)))
	   module)

	  (label-here! end-label)

	  (assembler-patch-code! (current-assembler)
				 (label-location start-label)
				 (- (label-location end-label)
				    (label-location start-label))
				 8)
	  (let ((code (assembler-get-code (current-assembler))))
	    (module-set-code! module code)
	    code)))))

(define (global-symbol symbol)
  (let ((index (global-symbol-index symbol)))
    `(,(* index 8) rbp))) 

(define mem:exit (global-symbol 'exit))

(define (lir:halt)
  (add-instruction! `(movq 0 rdi))
  (add-instruction! `(callq* ,mem:exit)))

(define module-current-procedure (make-parameter #f))
