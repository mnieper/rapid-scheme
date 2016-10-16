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

(define-record-type <global>
  (make-global name offset)
  global?
  (name global-name)
  (offset global-offset))

(define-record-type <reloc>
  (make-reloc offset name symbol addend)
  reloc?
  (offset reloc-offset)
  (name reloc-name)
  (symbol reloc-symbol)
  (addend reloc-addend))

(define-record-type <object-file-section>
  (%make-section name flags alignment size progbits globals relocs)
  object-file-section?
  (name section-name)
  (flags section-flags)
  (alignment section-alignment object-file-section-set-alignment!)
  (size section-size section-set-size!)
  (progbits section-progbits section-set-progbits!)
  (globals section-globals section-set-globals!)
  (relocs section-relocs section-set-relocs!))

(define (make-section name flags progbits)
  (%make-section name (list->flags flags) 1 0
		 (and progbits #u8()) '() '()))

(define (object-file-make-section object-file name flags progbits)
  (let ((section (make-section name flags progbits)))
    (append-section! object-file section)
    section))

(define (make-text-section object-file)
  (object-file-make-section object-file ".text" '(alloc execinstr) #t))

(define (make-data-section object-file)
  (object-file-make-section object-file ".data" '(alloc write) #t))

(define (make-bss-section object-file)
  (object-file-make-section object-file ".bss" '(alloc write) #f))

(define (object-file-section-set-size! section size)
  (section-set-size! section size)
  (when (section-progbits section)
    (section-set-progbits! section (make-bytevector size 0))))

(define (object-file-section-set-contents! section contents at)
  (bytevector-copy! (section-progbits section) at contents))

(define (object-file-section-add-global! section name offset)
  (section-set-globals! section
			(cons (make-global name offset)
			      (section-globals section))))

(define (object-file-section-add-reloc! section offset name symbol addend)
  (let
      ((name
	(case name
	  ((R_X86_64_64) "R_X86_64_64")
	  ((R_X86_64_32S) "R_X86_64_32S")
	  (else
	   (error "unknown reloc name" name)))))
    (section-set-relocs! section
			 (cons (make-reloc offset name symbol addend)
			       (section-relocs section)))))

(define (for-each-global proc section)
  (for-each proc (reverse (section-globals section))))

(define (for-each-reloc proc section)
  (for-each proc (reverse (section-relocs section))))

(define-record-type <object-file>
  (%make-object-file sections)
  object-file?
  (sections object-file-sections object-file-set-sections!)
  (text-section object-file-get-text-section set-text-section!)
  (data-section object-file-get-data-section set-data-section!)
  (bss-section object-file-get-bss-section set-bss-section!))

(define (append-section! object-file section)
  (object-file-set-sections! object-file
			     (cons section
				   (object-file-sections object-file))))

(define (for-each-section proc object-file)
  (for-each proc (reverse (object-file-sections object-file))))

(define (make-object-file)
  (let ((object-file (%make-object-file '())))
    (set-text-section! object-file (make-text-section object-file))
    (set-data-section! object-file (make-data-section object-file))
    (set-bss-section! object-file (make-bss-section object-file))
    object-file))

(define (output-object-file object-file filename)
  (with-output-to-file filename (lambda () (write-object-file object-file))))

(define (write-object-file object-file)
  (for-each-section
   (lambda (section)
     (unless (and (zero? (section-size section))
		  (null? (section-globals section)))
       (write-directive "section" (section-name section) (section-flags section)
			(if (section-progbits section)
			    "@progbits"
			    "@nobits"))
       (for-each-global
	(lambda (global)
	  (write-directive "global" (global-name global))
	  (write-directive "set"
			   (global-name global)
			   (string-append (section-name section)
					  " + "
					  (number->hex (global-offset global)))))
	section)
       (for-each-reloc
	(lambda (reloc)
	  (write-directive "reloc"
			   (number->hex (reloc-offset reloc))
			   (reloc-name reloc)
			   (string-append (reloc-symbol reloc)
					  " + "
					  (number->hex (reloc-addend reloc)))))
	section)
       (when (>= (section-alignment section) 2)
	 (write-directive "balign" (number->string (section-alignment section))))
       (let ((progbits (section-progbits section)))
	 (if progbits
	     (do ((i 0 (+ i 1)))
		 ((= i (bytevector-length progbits)))
	       (write-directive "byte" (number->hex
					(bytevector-u8-ref progbits i))))
	     (write-directive "zero" (number->string (section-size section)))))))
   object-file)
  (write-directive "end"))

(define (list->flags list)
  (string-append "\""
		 (let loop ((list list))
		   (if (null? list)
		       "\""
		       (string-append (case (car list)
					((alloc) "a")
					((write) "w")
					((execinstr) "x")
					(else
					 (error "unknown section flag" (car list))))
				      (loop (cdr list)))))))
