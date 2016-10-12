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
  (module reference-module reference-set-module!)
  (offset reference-offset reference-set-offset!))

(define (module-reference-address reference)
  (+ (module-offset (reference-module reference))
     (reference-offset reference)))

(define-record-type <module-procedure>
  (make-procedure)
  module-procedure?)

(define-record-type <module-datum>
  (make-datum label bytes)
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
  (offset module-offset module-set-offset!)
  (datums module-datums module-set-datums!)
  (vars module-vars module-set-vars!))

(define (make-module)
  (%make-module (make-assembler) '() '() #f))

#;(define (module-make-reference module)
  (make-reference module))

(define (for-each-datum proc module)
  (for-each proc (reverse (module-datums module))))

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
  (make-procedure))

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
	  (assemble `(quad 0))
	  (assemble `(quad ,(* 8 (var-count module))))

	  (for-each-datum
	   (lambda (datum)
	     (assembler-align! (current-assembler) 8)
	     (let ((label (assembler-make-label (current-assembler))))
	       ;; FIXME: HAVE TO ADD GC-MARK BIT TO DIFFERENCE
	       (assemble `(quad ,(- (label-location label)
				    (label-location start-label)))))
	     (module-reference-here! (datum-reference datum))
	     (do ((i 0 (+ i 1)))
		 ((= i (bytevector-length (datum-bytes datum))))
	       (assemble `(byte ,(bytevector-u8-ref (datum-bytes datum) i)))))
	   module)

	  (assembler-align! (current-assembler) 8)
	  (for-each-var
	   (lambda (var)
	     (module-reference-here! (var-reference var))
	     (assemble `(quad ,(var-init var))))
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
