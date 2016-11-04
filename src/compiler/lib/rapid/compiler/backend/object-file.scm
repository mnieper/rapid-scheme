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

(define (symbol->name symbol)
  (case symbol
    ((R_X86_64_64) "R_X86_64_64")
    ((R_X86_64_32S) "R_X86_64_32S")
    (else
     (error "unknown reloc name" symbol))))

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

;; TODO: Don't unpack all values
;; TODO: Use match in GAS

(define (write-section section)
  (match section
    ((program-section ,name (flags ,flags ...) (align ,alignment) (progbits ,progbits)
		      (globals ,globals ...) (relocs ,relocs ...))
     (%write-section name flags alignment (bytevector-length progbits) progbits globals relocs))
    
    ((text-section ,alignment ,progbits ,globals ,relocs)
     (write-section
      `(program-section ".text" (flags 'alloc execinstr) ,alignment ,progbits
			,globals ,relocs)))

    ((data-section ,alignment ,progbits ,globals ,relocs)
     (write-section
      `(program-section ".data" (flags 'alloc write) ,alignment ,progbits
			,globals ,relocs)))

    ((bss-section (align ,alignment) (size ,size) (globals ,globals) (relocs ,relocs))
     (%write-section ".bss" '(alloc execinstr) alignment size #f globals relocs))
    (,_ (error "invalid section" section))))


(define (%write-section name flags alignment size progbits globals relocs)

  (define (write-global global)
    (match global
      ((,global-name ,offset)
       (write-directive "global" global-name)
       (write-directive "set"
			global-name
			(string-append name " + " (number->hex offset))))
      (,_ (error "invalid global" global))))

  (define (write-reloc reloc)
    (match reloc
      ((,offset ,name ,symbol ,addend)
       (write-directive "reloc"
			(number->hex offset)
			(symbol->name name)
			(string-append symbol
				       " + "
				       (number->hex addend))))))
  
  (unless (and (zero? size)
	       (null? globals))
    (write-directive "section" name (list->flags flags)
		     (if progbits
			 "@progbits"
			 "@nobits"))
    (for-each write-global globals)
    (for-each write-reloc relocs)
    
    (when (>= alignment 2)
      (write-directive "balign" (number->string alignment)))
    (if progbits
	(do ((i 0 (+ i 1)))
	    ((= i (bytevector-length progbits)))
	  (write-directive "byte" (number->hex
				   (bytevector-u8-ref progbits i))))
	(write-directive "zero" size))))

;;; Object files

(define (output-object-file filename object-file)
  (with-output-to-file filename (lambda () (write-object-file object-file))))

(define (write-object-file object-file)
  (match object-file
    ((object-file ,section* ...)
     (for-each write-section section*))
    (,_ (error "invalid object file" object-file)))
  (write-directive "end"))



