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

(define (codegen-emit filename program)
  (match program
    ((program (modules ,module* ...)
	      (inits (,var* ,reference*) ...)
	      (entry ,entry))
     (%codegen-emit filename module* (map list var* reference*) entry))
    (,_ (error "invalid program" program))))

(define (%codegen-emit filename modules inits entry)
  (let-values (((offsets size) (get-module-offsets modules)))
    (define (module-offset module)
      (imap-ref offsets module))
    (define (reference-address reference)
      (+ (module-offset (module-reference-module reference))
	 (module-reference-offset reference)))
    (define (entry-global)
      `("rapid_run" ,(reference-address entry)))
    (define (init->reloc init)
      (let ((var (car init))
	    (reference (cadr init)))
	`(,(reference-address var)
	  R_X86_64_64
	  "rapid_text"
	  ,(reference-address reference))))
    (let ((progbits (make-bytevector size 0)))
      (for-each
       (lambda (module)
	 (bytevector-copy! progbits (module-offset module) (module-code module)))
       modules)
      (output-object-file
       filename
       `(object-file
	 (program-section
	  "rapid_text" (flags alloc write execinstr) (align 8)
	  (progbits ,progbits)
	  (globals ,(entry-global))
	  (relocs ,@(map init->reloc inits))))))))

(define (get-module-offsets modules)
  (let loop ((modules modules)
	     (offset 0)
	     (offsets (make-imap eq?)))
    (if (null? modules)
	(values offsets offset)
	(let ((module (car modules)))
	  (let ((code (module-code module)))
	    (loop (cdr modules)
		  (+ offset (align (+ offset (bytevector-length code)) 16))
		  (imap-replace offsets module offset)))))))

(define (align integer alignment)
  (let*
      ((alignment (if (zero? alignment)
		      1
		      alignment))
       (integer (+ integer alignment -1)))
    (- integer (remainder integer alignment))))

