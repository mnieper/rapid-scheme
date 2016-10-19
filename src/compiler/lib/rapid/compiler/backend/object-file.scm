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

;;; Globals

(define-record-type <global>
  (object-file-make-global name offset)
  object-file-global?
  (name global-name)
  (offset global-offset))

(define (write-global section global)
  (write-directive "global" (global-name global))
  (write-directive "set"
		   (global-name global)
		   (string-append (section-name section)
				  " + "
				  (number->hex (global-offset global)))))

;;; Relocs

(define-record-type <reloc>
  (make-reloc offset name symbol addend)
  object-file-reloc?
  (offset reloc-offset)
  (name reloc-name)
  (symbol reloc-symbol)
  (addend reloc-addend))

(define (object-file-make-reloc offset name symbol addend)
  (make-reloc offset (symbol->name name) symbol addend))

(define (symbol->name symbol)
  (case symbol
    ((R_X86_64_64) "R_X86_64_64")
    ((R_X86_64_32S) "R_X86_64_32S")
    (else
     (error "unknown reloc name" symbol))))

(define (write-reloc reloc)
  (write-directive "reloc"
		   (number->hex (reloc-offset reloc))
		   (reloc-name reloc)
		   (string-append (reloc-symbol reloc)
				  " + "
				  (number->hex (reloc-addend reloc)))))

;;; Sections

(define-record-type <object-file-section>
  (make-section name flags alignment size progbits globals relocs)
  object-file-section?
  (name section-name)
  (flags section-flags)
  (alignment section-alignment object-file-section-set-alignment!)
  (size section-size section-set-size!)
  (progbits section-progbits section-set-progbits!)
  (globals section-globals)
  (relocs section-relocs))

(define (object-file-make-section name flags alignment size progbits globals reloc)
  (make-section name (list->flags flags) alignment size progbits globals reloc))

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

(define (object-file-make-program-section name flags alignment progbits globals relocs)
  (object-file-make-section name flags
			    alignment (bytevector-length progbits) progbits globals relocs))

(define (object-file-make-text-section alignment progbits globals relocs)
  (object-file-make-program-section ".text" '(alloc execinstr)
			    alignment progbits globals relocs))

(define (object-file-make-data-section alignment progbits globals relocs)
  (object-file-make-program-section ".data" '(alloc write)
				    alignment progbits globals relocs))

(define (object-file-make-bss-section alignment size globals relocs)
  (object-file-make-section ".bss" '(alloc execinstr)
			    alignment size #f globals relocs))


(define (write-section section)
  (unless (and (zero? (section-size section))
	       (null? (section-globals section)))
    (write-directive "section" (section-name section) (section-flags section)
		     (if (section-progbits section)
			 "@progbits"
			 "@nobits"))
    (for-each (lambda (global) (write-global section global)) (section-globals section))
    (for-each write-reloc (section-relocs section))
    (when (>= (section-alignment section) 2)
      (write-directive "balign" (number->string (section-alignment section))))
    (let ((progbits (section-progbits section)))
      (if progbits
	  (do ((i 0 (+ i 1)))
	      ((= i (bytevector-length progbits)))
	    (write-directive "byte" (number->hex
				     (bytevector-u8-ref progbits i))))
	  (write-directive "zero" (number->string (section-size section)))))))
  
;;; Object files

(define (output-object-file filename sections)
  (with-output-to-file filename (lambda () (write-object-file sections))))

(define (write-object-file sections)
  (for-each write-section sections)
  (write-directive "end"))
