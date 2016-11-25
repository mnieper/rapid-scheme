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
    ((program ,declaration* ...)
     (receive (modules inits entry)
	 (let loop ((declaration* declaration*))
	   (if (null? declaration*)
	       (values '() '() #f)
	       (receive (modules inits entry)
		   (loop (cdr declaration*))		 
		 (match (car declaration*)
		   ((module ,name ,module-declaration* ...)
		    (values (cons (list name (make-module `(module ,@module-declaration*)))
				  modules)
			    inits
			    entry))
		   ((init ,var ,reference)
		    (values modules
			    (cons (list var reference) inits)
			    entry))
		   ((entry (,module ,name))
		    (values modules
			    inits
			    (list module name)))		  
		   (,_ (error "invalid program declaration" (car declaration*)))))))
       (%codegen-emit filename modules inits entry)))
     (,_ (error "invalid program" program))))

(define (%codegen-emit filename modules inits entry)
  (let-values (((offsets size) (get-module-offsets modules)))
    (define (module-offset module)
      (imap-ref offsets module))
    (define (reference-address reference)
      (let ((module (cadr (assq (car reference) modules)))
	    (offset (module-offset (car reference))))
	(+ offset (module-label-offset module (cadr reference)))))
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
	 (bytevector-copy! progbits
			   (module-offset (car module))
			   (module-code (cadr module))))
       modules)
      (for-each
       (lambda (init)
	 (let ((var (car init)))
	   (bytevector-integer-set! progbits (reference-address var) 0 8)))
       inits)      
      (output-object-file
       filename
       `(object-file
	 (program-section
	  "rapid_text" (flags alloc write execinstr) (align 16)
	  (progbits ,progbits)
	  (globals ,(entry-global))
	  (relocs ,@(map init->reloc inits))))))))

(define (get-module-offsets modules)
  (let loop ((modules modules)
	     (offset 0)
	     (offsets (make-imap eq?)))
    (if (null? modules)
	(values offsets offset)
	(let ((name (caar modules))
	      (module (cadar modules)))
	  (let ((code (module-code module)))
	    (loop (cdr modules)
		  (+ offset (align (+ offset (bytevector-length code)) 16))
		  (imap-replace offsets name offset)))))))

(define (align integer alignment)
  (let*
      ((alignment (if (zero? alignment)
		      1
		      alignment))
       (integer (+ integer alignment -1)))
    (- integer (remainder integer alignment))))

