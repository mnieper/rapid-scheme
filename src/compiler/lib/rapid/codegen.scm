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

;;; Layout of a module in assembly
;;; --- HEADER ---
;;; 0-7: Length of module    (calculated by assembler)
;;; 8-15: offset of vars     (calculated by assembler)
;;; --- PROCEDURE ---
;;; 16-23: POINTER TO HEADER
;;; 24: --- procedure entry point ---
;;;
;;; ...

(define-record-type <codegen>
  (%make-codegen modules relocs)
  codegen?
  (modules codegen-modules codegen-set-modules!)
  (relocs codegen-relocs codegen-set-relocs!))

(define-record-type <reloc>
  (make-reloc var reference)
  reloc?
  (var reloc-var)
  (reference reloc-reference))

(define (for-each-module proc codegen)
  (for-each proc (reverse (codegen-modules codegen))))

(define (for-each-reloc proc codegen)
  (for-each proc (reverse (codegen-relocs codegen))))

(define (make-codegen)
  (%make-codegen '() '()))

(define (codegen-add-module! codegen module)
  (codegen-set-modules! codegen
			(cons module
			      (codegen-modules codegen))))

(define (codegen-var-set! codegen var reference)
  (codegen-set-relocs! codegen
		       (cons (make-reloc var reference)
			     (codegen-relocs codegen))))
	 
(define (codegen-emit codegen filename entry)
  (define offset 0)
  (define object-file (make-object-file))
  (define rapid-text-section
    (object-file-make-section object-file
			      "rapid_text"
			      '(alloc write execinstr)
			      #t))

  (for-each-module
   (lambda (module)
     (let ((code (module-get-code module)))
       (module-set-offset! module offset)
       (set! offset (align (+ offset (bytevector-length code)) 16))))
   codegen)
    
  (object-file-section-set-alignment! rapid-text-section 8)
  (object-file-section-set-size! rapid-text-section offset)

  (for-each-reloc
   (lambda (reloc)
     (let ((var (reloc-var reloc))
	   (reference (reloc-reference reloc)))
     (object-file-section-add-reloc! rapid-text-section
				     (module-reference-address
				      (module-var-reference var))
				     'R_X86_64_64
				     "rapid_text"
				     (module-reference-address reference))))
   codegen)
  
  (for-each-module
   (lambda (module)
     (let ((code (module-get-code module))
	   (offset (module-offset module)))
       (object-file-section-set-contents! rapid-text-section code offset)))
   codegen)
  
  (object-file-section-add-global! rapid-text-section
				   "rapid_run"
				   (module-reference-address entry))
  (output-object-file object-file filename))

(define (align integer alignment)
  (let*
      ((alignment (if (zero? alignment)
		      1
		      alignment))
       (integer (+ integer alignment -1)))
    (- integer (remainder integer alignment))))

