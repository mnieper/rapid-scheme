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

(define-record-type <elf-object>
  (%make-elf-object sections strtab shstrtab
		    text-section data-section bss-section)
  elf-object?
  (sections elf-object-sections elf-object-set-sections!)
  (strtab elf-object-strtab)
  (shstrtab elf-object-shstrtab)
  (text-section elf-object-text-section)
  (data-section elf-object-data-section)
  (bss-section elf-object-bss-section))

(define (elf-object-add-section! elf-object section)
  (elf-object-set-sections! elf-object
			    (cons section
				  (elf-object-sections elf-object))))
				  
(define (elf-object-get-sections elf-object)
  (reverse (elf-object-sections elf-object)))

(define-record-type <elf-object-section>
  (%make-section name type flags addralign progbits)
  elf-object-section?
  (name section-name)
  (type section-type)
  (flags section-flags)
  (addralign section-addralign)
  (progbits section-progbits)
  (offset section-offset section-set-offset!))

(define (make-section name type flags)
  (%make-section name type flags 1 #u8()))

(define-record-type <elf-object-location>
  (make-elf-object-symbol)
  elf-object-symbol?)

(define-record-type <strtab>
  (%make-strtab port offset)
  strtab?
  (port strtab-port)
  (offset strtab-offset strtab-set-offset!))

(define (make-strtab)
  (let ((port (open-output-bytevector)))
    (write-u8 0 port)
    (%make-strtab port 1)))

(define (strtab-add-string strtab name)
  (let ((bytevector (string->utf8 name)))
    (write-bytevector bytevector (strtab-port strtab))
    (write-u8 0 (strtab-port strtab))
    (let ((offset (strtab-offset strtab)))
      (strtab-set-offset! strtab (+ offset (bytevector-length bytevector) 1))
      offset)))

(define (make-elf-object)
  (define strtab (make-strtab))
  (define shstrtab (make-strtab))
  (define symtab-name (strtab-add-string shstrtab ".symtab"))
  (define strtab-name (strtab-add-string shstrtab ".strtab"))
  (define shstrtab-name (strtab-add-string shstrtab ".shstrtab"))
  (define text-name (strtab-add-string shstrtab ".text"))
  (define data-name (strtab-add-string shstrtab ".data"))
  (define bss-name (strtab-add-string shstrtab ".bss"))
  (define null-section
    (make-section 0 sh-type/null 0))
  (define text-section
    (make-section text-name sh-type/progbits (+ sh-flags/alloc sh-flags/execinstr)))
  (define data-section
    (make-section data-name sh-type/progbits (+ sh-flags/write sh-flags/alloc)))
  (define bss-section
    (make-section bss-name sh-type/nobits (+ sh-flags/write sh-flags/alloc)))  
  (define elf-object
    (%make-elf-object '()
		      strtab
		      shstrtab
		      text-section
		      data-section
		      bss-section))
  (elf-object-add-section! elf-object null-section)
  (elf-object-add-section! elf-object text-section)
  (elf-object-add-section! elf-object data-section)
  (elf-object-add-section! elf-object bss-section)
  elf-object)

(define (elf-object-section-name elf-object name)
  (strtab-add-string (elf-object-shstrtab elf-object) name))

(define (elf-object-section elf-object name flags)
  (define %flags
    (let loop ((flags flags))
      (if (null? flags)
	  0
	  (+ (case (car flags)
	       ((write) sh-flags/write)
	       ((alloc) sh-flags/alloc)
	       ((execinstr) sh-flags/execinstr)
	       (else
		(error "elf-object-section: unknown flag" (car flags))))
	     (loop (cdr flags))))))
  (let ((section
	 (make-section (elf-object-section-name elf-object name)
		       sh-type/progbits
		       %flags)))
    (elf-object-add-section! elf-object section)
    section))

(define write-elf-object
  (case-lambda
   ((elf-object) (%write-elf-object (current-output-port)))
   ((elf-object port) (%write-elf-object elf-object port))))

(define (%write-elf-object elf-object port)
  (define sections (elf-object-get-sections elf-object))
  (define shstrndx (length sections))
  (define section-header-offset (get-section-header-offset sections))
  (write-elf-header elf-type/rel
		    elf-machine/x86_64
		    elf-version/current
		    0 0 section-header-offset 0 64 0 0
		    64 (length sections) shstrndx
		    port)
  ;; FIXME: Write the rest // should be doable
  )

(define (get-section-header-offset sections)
  (let loop ((sections sections) (offset 64))
    (if (null? sections)
	(align offset 8)
	(let*
	    ((section (car sections))
	     (offset (align offset (section-addralign section))))
	  (section-set-offset! section offset)
	  (loop (cdr sections)
		(+ offset (bytevector-length (section-progbits section))))))))

(define (write-elf-header type machine version entry phoff shoff flags ehsize
			  phentsize phnum shentsize shnum shstrndx port)
  (write-elf-ident elf-class/64 elf-data/2lsb version elf-osabi/none port)
  (write-word type port)
  (write-word machine port)
  (write-long version port)
  (write-quad entry port)
  (write-quad phoff port)
  (write-quad shoff port)
  (write-long flags port)
  (write-word ehsize port)
  (write-word phentsize port)
  (write-word phnum port)
  (write-word shentsize port)
  (write-word shnum port)
  (write-word shstrndx port))

(define (write-elf-ident class data version osabi port)
  (let ((ident (make-bytevector elf-ident/nident 0)))
    (bytevector-u8-set! ident elf-ident/mag0 elf-mag/0)
    (bytevector-u8-set! ident elf-ident/mag1 elf-mag/1)
    (bytevector-u8-set! ident elf-ident/mag2 elf-mag/2)
    (bytevector-u8-set! ident elf-ident/mag3 elf-mag/3)
    (bytevector-u8-set! ident elf-ident/class class)
    (bytevector-u8-set! ident elf-ident/data data)
    (bytevector-u8-set! ident elf-ident/version version)
    (bytevector-u8-set! ident elf-ident/osabi osabi)
    (bytevector-u8-set! ident elf-ident/abiversion 0)
    (write-bytevector ident port)))

(define (align integer alignment)
  (let*
      ((alignment (if (zero? alignment)
		      1
		      alignment))
       (integer (+ integer alignment -1)))
    (- integer (remainder integer alignment))))

(define elf-ident/mag0 0)
(define elf-ident/mag1 1)
(define elf-ident/mag2 2)
(define elf-ident/mag3 3)
(define elf-ident/class 4)
(define elf-ident/data 5)
(define elf-ident/version 6)
(define elf-ident/osabi 7)
(define elf-ident/abiversion 8)
(define elf-ident/pad 9)
(define elf-ident/nident 16)

(define elf-mag/0 #x7F)
(define elf-mag/1 #x45)
(define elf-mag/2 #x4c)
(define elf-mag/3 #x46)

(define elf-class/none 0)
(define elf-class/32 1)
(define elf-class/64 2)

(define elf-data/none 0)
(define elf-data/2lsb 1)
(define elf-data/2msb 2)

(define elf-version/none 0)
(define elf-version/current 1)

(define elf-osabi/none 0)

(define elf-type/none 0)
(define elf-type/rel 1)
(define elf-type/exec 2)
(define elf-type/dyn 3)
(define elf-type/core 4)

(define elf-machine/none 0)
(define elf-machine/x86_64 62)

(define sh-type/null 0)
(define sh-type/progbits 1)
(define sh-type/symtab 2)
(define sh-type/strtab 3)
(define sh-type/shtrela 4)
(define sh-type/hash 5)
(define sh-type/dynamic 6)
(define sh-type/note 7)
(define sh-type/nobits 8)
(define sh-type/rel 9)
(define sh-type/shlib 10)
(define sh-type/dynsym 11)

(define sh-flags/write 1)
(define sh-flags/alloc 2)
(define sh-flags/execinstr 4)
