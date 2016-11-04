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

(define (compile-section section)
  (match section
    ((program-section ,name (flags ,flags ...) (align ,alignment) (progbits ,progbits)
		      (globals ,globals ...) (relocs ,relocs ...))
     (%compile-section name flags alignment (bytevector-length progbits) progbits globals relocs))
    
    ((text-section ,alignment ,progbits ,globals ,relocs)
     (compile-section
      `(program-section ".text" (flags 'alloc execinstr) ,alignment ,progbits
			,globals ,relocs)))

    ((data-section ,alignment ,progbits ,globals ,relocs)
     (compile-section
      `(program-section ".data" (flags 'alloc write) ,alignment ,progbits
			,globals ,relocs)))

    ((bss-section (align ,alignment) (size ,size) (globals ,globals) (relocs ,relocs))
     (%compile-section ".bss" '(alloc execinstr) alignment size #f globals relocs))
    (,_ (error "invalid section" section))))

(define (%compile-section name flags alignment size progbits globals relocs)

  (define (compile-global global)
    (match global
      ((,global-name ,offset)
       `(begin
	  (global ,global-name)
	  (set ,global-name
	       ,(string-append name " + " (number->hex offset)))))
      (,_ (error "invalid global" global))))

  (define (compile-reloc reloc)
    (match reloc
      ((,offset ,name ,symbol ,addend)
       `(reloc
	 ,(number->hex offset)
	 ,(symbol->name name)
	 ,(string-append symbol
			 " + "
			 (number->hex addend))))))

  `(begin
     ,@(if (and (zero? size)
		(null? globals))
	   '()        
	   `((section ,name ,(list->flags flags) ,(if progbits "@progbits" "@nobits"))
	     ,@(map compile-global globals)
	     ,@(map compile-reloc relocs)
	     ,@(if (>= alignment 2)
		   `((balign ,(number->string alignment)))
		   '())
	     ,@(if progbits
		   (let loop ((i 0))
		     (if (= i (bytevector-length progbits))
			 '()
			 `((byte ,(number->hex (bytevector-u8-ref progbits i)))
			   . ,(loop (+ i 1)))))
		   `((zero ,size)))))))

;;; Object files

(define (output-object-file filename object-file)
  (let ((assembly (compile-object-file object-file)))
    (output-gas-assembly filename assembly)))

(define (compile-object-file object-file)
  `(,@(match object-file
	((object-file ,section* ...) (map compile-section section*))
	(,_ (error "invalid object file" object-file)))
    (end)))



