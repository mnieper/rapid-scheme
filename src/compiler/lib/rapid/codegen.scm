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


(define-record-type <codegen-label>
  (make-label assembler-label)
  codegen-label?
  (assembler-label label-assembler-label))

(define-record-type <codegen-module>
  (make-module datums vars)
  codegen-module?
  (datums module-datums module-set-datums!)
  (vars module-vars module-set-vars!))

(define-record-type <codegen-module-procedure>
  (make-procedure)
  codegen-module-procedure?)

;; TO BE EXPORTED???
(define-record-type <codegen-module-datum>
  (make-datum label bytes)
  codegen-module-datum?
  (label datum-label)
  (bytes datum-bytes))

(define-record-type <codegen-module-var>
  (make-var init)
  codegen-module-var?
  (init var-init))

(define-record-type <codegen>
  (%make-codegen assembler modules)
  codegen?
  (assembler codegen-assembler)
  (modules codegen-modules codegen-set-modules!))

(define (for-each-module proc codegen)
  (for-each proc (reverse (codegen-modules codegen))))

(define (make-codegen)
  (%make-codegen (make-assembler) '()))

(define (codegen-make-label codegen)
  (make-label (assembler-make-label (codegen-assembler codegen))))

(define (codegen-add-module codegen)
  (let ((module (make-module '() '())))
    (codegen-set-modules! codegen
			  (cons module
				(codegen-modules codegen)))
    module))

(define (for-each-datum proc module)
  (for-each proc (reverse (module-datums module))))

(define (for-each-var proc module)
  (for-each proc (reverse (module-vars module))))

(define (var-count module)
  (length (module-vars module)))

(define (codegen-module-add-datum! module label bytes)
  (module-set-datums! module
		      (cons (make-datum label bytes)
			    (module-datums module))))

(define (codegen-module-add-procedure! codegen label)
  (make-procedure))

(define (codegen-module-add-var codegen init)
  (let ((var (make-var init)))
    (module-set-vars! module
		      (cons var
			    (module-vars module)))
    var))

(define (codegen-emit codegen filename label)
  (define object-file (make-object-file))
  (parameterize ((current-assembler (codegen-assembler codegen)))
    (define rapid-text-section
      (object-file-make-section object-file
				"rapid_text"
				'(alloc write execinstr)
				#t))

    (for-each-module
     (lambda (module)    
       (define start-label (assembler-make-label (current-assembler)))
       (define end-label (assembler-make-label (current-assembler)))

       (assembler-align! (current-assembler) 8)
       (label-here! start-label)
       (assemble `(quad 0))
       (assemble `(quad ,(* 8 (var-count module))))
       
       (for-each-datum
	(lambda (datum)
	  (assembler-align! (current-assembler) 8)
	  (let ((label (assembler-make-label (current-assembler))))
	    ;; FIXME: HAVE TO ADD GC-MARK BIT TO DIFFERENCE
	    (assemble `(quad ,(- (label-location label)
				 (label-location start-label)))))
	  (label-here! (label-assembler-label (datum-label datum)))
	  (do ((i 0 (+ i 1)))
	      ((= i (bytevector-length (datum-bytes datum))))
	    (assemble `(byte ,(bytevector-u8-ref (datum-bytes datum) i)))))
	module)

       ;; ADD VARS! (init is either a quad or a label of this codegen!)
       
       (label-here! end-label)

       (assembler-patch-code! (current-assembler)
			      (label-location start-label)
			      (- (label-location end-label)
				 (label-location start-label))
			      8))
     codegen)
    
    (let ((code (assembler-get-code (current-assembler))))
      (object-file-section-set-alignment! rapid-text-section 8)
      (object-file-section-set-size! rapid-text-section
				     (bytevector-length code))
      (object-file-section-set-contents! rapid-text-section code 0)
      (object-file-section-add-global! rapid-text-section
				       "rapid_run"
				       (label-location
					(label-assembler-label label)))))
  
  (output-object-file object-file filename))
