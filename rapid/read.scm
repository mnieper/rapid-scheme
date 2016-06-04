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

(define-record-type <source-port>
  (%make-source-port port source ci? line column)
  source-port?
  (port source-port-port)
  (source source-port-source)
  (ci? source-port-ci? source-port-set-ci?!)
  (line source-port-line source-port-set-line!)
  (column source-port-column source-port-set-column!))

(define (make-source-port port source ci?)
  (%make-source-port port source ci? 1 0))

(define (source-port-peek-char source-port)
  (peek-char (source-port-port source-port)))

(define (source-port-read-char source-port)
  (let ((char (read-char (source-port-port source-port))))
    (cond
     ((eof-object? char) char)
     (else
      (case char
	((#\tab)
	 (source-port-set-column! source-port
				  (* (+ (quotient
					 (source-port-column source-port) 8)
					1)
				     8)))
	((#\newline)
	 (source-port-set-column! source-port 0)
	 (source-port-set-line! source-port
				(+ (source-port-line source-port) 1)))
	((#\return)
	 (source-port-set-column! source-port 0))
	(else
	 ;; FIXME: Handle Unicode character width correctly.
	 ;; See also <https://www.gnu.org/prep/standards/standards.html#Errors>.
	 (source-port-set-column! source-port
				  (+ (source-port-column source-port) 1))))
      char))))

(define (source-port-fold-case! source-port)
  (source-port-set-ci?! source-port #t))

(define (source-port-no-fold-case! source-port)
  (source-port-set-ci?! source-port #f))

(define (source-port-position source-port)
  (vector (source-port-line source-port)
	  (source-port-column source-port)))

(define (source-port-make-location source-port start end)
  (make-source-location (source-port-source source-port) start end))

(define (read-syntax source-port context)

  (define source (source-port-source source-port))

  (define (peek) (source-port-peek-char source-port))

  (define (read)
    (let ((char (source-port-read-char source-port)))
      (if (eof-object? char)
	  (raise char)
	  char)))

  (define (fold-case!) (source-port-fold-case! source-port))

  (define (no-fold-case!) (source-port-no-fold-case! source-port))

  (define (ci?) (source-port-ci? source-port))
  
  (define (string->identifier string)
    (let ((string (if (ci?) (string-foldcase string) string)))
      (string->symbol string)))
  
  (define start (make-parameter #f))

  (define (position) (source-port-position source-port))

  (define (location)
    (make-source-location source (start) (position)))

  (define syntax
    (case-lambda
     ((datum)
      (syntax datum #f))
     ((datum reference)
      (make-syntax datum (location) context reference)))) 

  (define (reader-error message . obj*)
    (apply raise-syntax-error (syntax #f) message obj*))

  (define (delimiter? char)
    (case char
      ((#\space #\tab #\return #\newline #\| #\( #\) #\" #\;)
       #t)
      (else
       (eof-object? char))))
  
  (define (read-token)
    (list->string
     (let loop ()
       (cond
	((delimiter? (peek))
	 '())
	(else
	 (let ((char (read)))
	   (cons char (loop))))))))

  (call-with-current-continuation
   (lambda (return)
     (define (with-eof-handler handler thunk) 
       (with-exception-handler
	(lambda (condition)
	  (cond
	   ((eof-object? condition)
	    (handler)
	    (return condition))
	   (else
	    (raise-continuable condition))))
	thunk))
     
     (define (read-directive)
       (let ((token (read-token)))
	 (case (string->symbol token)
	   ((fold-case)
	    (fold-case!))
	   ((no-fold-case)
	    (no-fold-case!))
	   (else
	    (reader-error "invalid directive ‘~a’" token)))))

     (define (read-boolean)
       (let ((token (read-token)))
	 (case (string->symbol token)
	   ((t true)
	    (syntax #true))
	   ((f false)
	    (syntax #false))
	   (else
	    (reader-error "invalid boolean ‘~a’" token)))))
     
     (define (read-nested-comment)
       (with-eof-handler
	(lambda ()
	  (reader-error "unterminated nested comment"))
	(lambda ()
	  (let loop ()
	    (case (read)
	      ((#\#)
	       (when (char=? (read) #\|)
		 (loop))
	       (loop))
	      ((#\|)
	       (unless (char=? (read) #\#)
		 (loop)))
	      (else
	       (loop)))))))

     (define (hex-digit char)
       (cond
	((char<=? #\0 char #\9)
	 (- (char->integer char) #x30))
	((char<=? #\A char #\F)
	 (- (char->integer char) #x37))
	((char<? #\a char #\f)
	 (- (char->integer char) #x57))
	(else
	 #f)))
     
     (define (read-hex-scalar-value)
       (let loop ((value #f))
	 (cond
	  ((hex-digit (peek))
	   => (lambda (digit)
		(read)
		(loop (+ (* 16 (or value 0)) digit))))
	  (else
	   value))))
	     
     (define (read-escape)
       (case (read)
	 ((#\a)
	  #\alarm)
	 ((#\b)
	  #\backspace)
	 ((#\t)
	  #\tab)
	 ((#\n)
	  #\newline)
	 ((#\r)
	  #\return)
	 ((#\")
	  #\")
	 ((#\|)
	  #\|)
	 ((#\x)
	  (cond
	   ((read-hex-scalar-value)
	    => (lambda (value)
		 (cond		 
		  ((parameterize ((start (position)))
		     (case (read)
		       ((#\;)
			value)
		       (else
			=> (lambda (char)
			     (error "semicolon expected, but found ‘~a’" char)
			     #f))))
		   => (lambda (value)
			(cond
			 ((or (< 0 value #xD7FF)
			      (< #xE000 value #x10FFFF))
			  (integer->char value))
			 (else
			  (reader-error "not a valid unicode code point ‘~a’" value)
			  #f))))
		  (else
		   #f))))
	   (else
	    (reader-error "hex scalar value expected"))))

	 ((#\space #\tab #\return #\newline)
	  => (lambda (char)
	       (let loop ((char char))
		 (case char
		   ((#\space #\tab)
		    (loop (read)))
		   ((#\return #\newline)
		    => (lambda (char)
			 (when (and (char=? char #\return)
				    (char=? (peek) #\newline))
			   (read))
			 (let loop ()
			   (case (peek)
			     ((#\space #\tab)
			      (read)
			      (loop))))))
		   (else
		    (reader-error "unexpected character ‘~a’ before line ending"
				  char))))
	       #f))
	 (else
	  => (lambda (char)
	       char))))
     
     (define (read-string)
       (with-eof-handler
	(lambda ()
	  (reader-error "unterminated string"))
	(lambda ()
	  (syntax
	   (list->string
	    (parameterize ((start #f))
	      (let loop ()
		(start (position))
		(case (read)
		  ((#\")
		   '())
		  ((#\\)
		   (let ((char (read-escape)))
		     (if char
			 (cons char (loop))
			 (loop))))
		  ((#\return)
		   (when (char=? (peek) #\newline)
		     (read))
		   (cons #\newline (loop)))
		  (else
		   => (lambda (char)
			(cons char (loop))))))))))))

     (define (read-identifier)
       (with-eof-handler
	(lambda ()
	  (reader-error "unterminated identifier"))
	(lambda ()
	  (syntax
	   (string->identifier
	    (list->string
	     (parameterize ((start #f))
	       (let loop ()
		 (start (position))
		 (case (read)
		   ((#\|)
		    '())
		   ((#\\)
		    (let
			((char
			  (case (peek)
			    ((#\space #\tab #\return #\newline)
			     (read))
			    (else
			     (read-escape)))))
		      (if char
			  (cons char (loop))
			  (loop))))
		   (else
		    => (lambda (char)
			 (cons char (loop)))))))))))))
     
     (with-eof-handler
      (lambda () (return (eof-object)))
      (lambda ()
	(let loop ()
	  (start (position))
	  (case (read)
	    ;; Skip whitespace
	    ((#\newline #\return #\space #\tab)
	     (loop))
	    ;; Skip line comments
	    ((#\;)
	     (let loop ()
	       (case (read)
		 ((#\newline)
		  #f)
		 ((#\return)
		  (when (char=? (peek) #\newline)
		    (read)))
		 (else
		  (loop))))
	     (loop))
	    ;; Strings
	    ((#\")
	     (read-string))
	    ;; Identifiers enclosed in vertical lines
	    ((#\|)
	     (read-identifier))
	    ;; Sharp syntax
	    ((#\#)
	     (with-eof-handler
	      (lambda ()
		(reader-error "incomplete sharp syntax at end of input"))
	      (lambda ()
		(case (peek)
		  ;; Booleans
		  ((#\t #\f)
		   (read-boolean))
		  (else		
		   (case (read)
		     ;; Nested comment
		     ((#\|)
		      (read-nested-comment)
		      (loop))
		     ;; Datum comment
		     ((#\;)
		      (parameterize ((start #f)) (loop))
		      (loop))
		     ;; Directives
		     ((#\!)
		      (read-directive)
		      (loop))
		     (else
		      => (lambda (char)
			   (reader-error "invalid sharp syntax ‘#~a’" char)
			   (loop)))))))))
	    ;; Invalid character
	    (else
	     => (lambda (char)
		  (reader-error "unexpected character ‘~a’ in input" char)
		  (loop))))))))))
