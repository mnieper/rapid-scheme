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

(define (delimiter? char)
  (case char
    ((#\space #\tab #\return #\newline #\| #\( #\) #\" #\;)
     #t)
    (else
     (eof-object? char))))

(define (initial? char)
  (or (char<=? #\A char #\Z)
      (char<=? #\a char #\z)
      (case char
	((#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\@)
	 #t)
	(else #f))))

(define (subsequent? char)
  (or (initial? char)
      (char<=? #\0 char #\9)
      (case char
	((#\+ #\- #\.)
	 #t)
	(else
	 #f))))

(define (sign? char)
  (or (char=? char #\+)
      (char=? char #\-)))

(define (sign-subsequent? char)
  (or (initial? char) (sign? char)))

(define (dot-subsequent? char)
  (or (sign-subsequent? char)
      (char=? char #\.)))

(define (hex-digit char)
  (cond
   ((char<=? #\0 char #\9)
    (- (char->integer char) #x30))
   ((char<=? #\A char #\F)
    (- (char->integer char) #x37))
   ((char<=? #\a char #\f)
    (- (char->integer char) #x57))
   (else
    #f)))

(define (make-label)
  (vector #f (list '())))
(define (label-syntax label)
  (vector-ref label 0))
(define (label-references label)
  (car (vector-ref label 1)))
(define (label-set-syntax! label syntax)
  (vector-set! label 0 syntax))
(define (label-add-reference! label reference)
  (let ((reference** (vector-ref label 1)))
    (set-car! reference** (cons reference (car reference**)))))
(define (label-delete-references! label)
  (vector-set! label 1 #f))
(define (label-add-references! label references)
  (vector-set! label 1 (cons references (vector-ref label 1))))
(define (label-for-each-reference proc label)
  (for-each
   (lambda (references)
     (for-each proc references))
   (vector-ref label 1)))

(define label-comparator (make-comparator integer? = < #f))

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
      (symbol->identifier (string->symbol string))))
  
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

  (define (hex-scalar-value token)
    (and-let*
	(((> (string-length token) 0))
	 (char (string-ref token 0))
	 ((or (char=? char #\x) (char=? char #\X))))	  
      (let loop ((value #f) (digits (cdr (string->list token))))
	(cond
	 ((null? digits)
	  value)
	 ((hex-digit (car digits))
	  => (lambda (digit)
	       (loop (+ (* 16 (or value 0)) digit) (cdr digits))))
	 (else
	  #f)))))
  
  (define (read-token)
    (list->string
     (let loop ()
       (cond
	((delimiter? (peek))
	 '())
	(else
	 (let ((char (read)))
	   (cons char (loop))))))))

  (define current-dot-handler
    (make-parameter (lambda ()
		      (reader-error "unexpected dot in source")
		      #f)))

  (define current-closing-parenthesis-handler
    (make-parameter (lambda ()
		      (reader-error "unexpected closing parenthesis in source")
		      #f)))

  (define (read-number)
    (let loop ((value #f))
      (if (char<=? #\0 (peek) #\9)
	  (loop (+ (* 10 (or value 0)) (- (char->integer (read)) #x30)))
	  value)))

  (define (read-hex-scalar-value)
    (let loop ((value #f))
      (cond
       ((hex-digit (peek))
	=> (lambda (digit)
	     (read)
	     (loop (+ (* 16 (or value 0)) digit))))
       (else
	value))))
  
  (define (code-point->character value)
    (cond
     ((or (<= 0 value #xD7FF)
	  (<= #xE000 value #x10FFFF))
      (integer->char value))
     (else
      (reader-error "not a valid unicode code point ‘~a’" value)
      #f)))

  (define (read-directive)
    (let ((token (read-token)))
      (case (string->symbol (string-foldcase token))
	((fold-case)
	 (fold-case!))
	((no-fold-case)
	 (no-fold-case!))
	(else
	 (reader-error "invalid directive ‘~a’" token)))))

  (define (read-boolean)
    (let ((token (read-token)))
      (case (string->symbol (string-foldcase token))
	((t true)
	 (syntax #t))
	((f false)
	 (syntax #f))
	(else
	 (reader-error "invalid boolean ‘~a’" token)))))
  
  (define (read-character)
    (let*
	((char (read))
	 (token (string-append (string char) (read-token))))
      (cond
       ((= (string-length token) 1)
	(syntax (string-ref token 0)))
       ((hex-scalar-value token)
	=> (lambda (value)
	     (let ((char (code-point->character value)))
	       (if char
		   (syntax char)
		   char))))
       (else
	(case (string->symbol (if (ci?) (string-foldcase token) token))
	  ((alarm) (syntax #\alarm))
	  ((backspace) (syntax #\backspace))
	  ((delete) (syntax #\delete))
	  ((escape) (syntax #\escape))
	  ((newline) (syntax #\newline))
	  ((null) (syntax #\null))
	  ((return) (syntax #\return))
	  ((space) (syntax #\space))
	  ((tab) (syntax #\tab))
	  (else
	   (reader-error "invalid character name ‘~a’" token)
	   #f))))))

  (define (check-identifier token)
    (string-for-each (lambda (char)
		       (unless (or (subsequent? char))
			 (reader-error "unexpected character in identifier ‘~a’"
				       char)))
		     token))
  
  (define (number token)
    (cond
     ((string->number token)
      => syntax)
     (else
      (reader-error "invalid number")
      #f)))
  
  (define (read-identifier)
    (let ((token (read-token)))
      (cond
       ((string->number token)
	=> syntax)
       (else
	(check-identifier token)
	(syntax (string->identifier token))))))

  (define (read-peculiar-identifier)
    (let ((token (read-token)))
      (cond
       ((string->number token)
	=> syntax)
       ((string=? token ".")
	((current-dot-handler)))
       (else
	(if (and (>= (string-length token) 2)
		 (or (and (sign? (string-ref token 0))
			  (if (char=? (string-ref token 1) #\.)
			      (or (= (string-length token) 2)
				  (not (dot-subsequent? (string-ref token 2)))))
			      (not (sign-subsequent? (string-ref token 1))))
		     (and (char=? (string-ref token 0) #\.)
			  (not (dot-subsequent? (string-ref token 1))))))
	    (reader-error "invalid peculiar identifier ‘~a’" token)
	    (check-identifier token))
	(syntax (string->identifier token))))))
  
  (define (read-escape)
    (case (read)
      ((#\a) #\alarm)
      ((#\b) #\backspace)
      ((#\t) #\tab)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\") #\")
      ((#\|) #\|)
      ((#\x #\X)
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
			  (reader-error "semicolon expected, but found ‘~a’" char)
			  #f))))
		=> code-point->character)
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

  (call-with-current-continuation
   (lambda (return)
     (define (with-eof-handler handler thunk) 
       (with-exception-handler
	(lambda (condition)
	  (cond
	   ((eof-object? condition)
	    (handler)
	    (return #f))
	   (else
	    (raise-continuable condition))))
	thunk))
     
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

     (define (read-symbol)
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

     (define (read-vector)
       (with-eof-handler
	(lambda ()
	  (reader-error "unterminated vector"))
	(lambda ()
	  (syntax	  
	   (call-with-current-continuation
	    (lambda (return)
	      (parameterize ((current-closing-parenthesis-handler #f))	     
		(let loop ((syntax* '()))
		  (current-closing-parenthesis-handler
		   (lambda ()
		     (return (list->vector (reverse syntax*)))))
		  (loop (cons (parameterize
				  ((start #f))
				(read-syntax))
			      syntax*))))))))))

     (define (read-bytevector)
       (cond
	((and (char=? (read) #\8) (char=? (read) #\())
	 (with-eof-handler
	  (lambda ()
	    (reader-error "unterminated bytevector"))
	  (lambda ()
	    (syntax
	     (call-with-current-continuation
	      (lambda (return)
		(parameterize ((current-closing-parenthesis-handler #f))	     
		  (let loop ((datum* '()))
		    (current-closing-parenthesis-handler
		     (lambda ()
		       (return (apply bytevector (reverse datum*)))))
		    (parameterize
			((start #f))
		      (let ((datum (unwrap-syntax (read-syntax))))
			(cond
			 ((and (exact-integer? datum)
			       (<= 0 datum 255))
			  (loop (cons datum datum*)))
			 (else
			  (reader-error "not a byte")
			  (loop datum*)))))))))))))
	(else
	 (reader-error "invalid bytevector")
	 #f)))

     (define (read-list)
       (with-eof-handler
	(lambda ()
	  (reader-error "unterminated list"))
	(lambda ()
	  (syntax
	   (call-with-current-continuation
	    (lambda (return)
	      (let*
		  ((syntax*
		    (call-with-current-continuation
		     (lambda (k)		   
		       (parameterize ((current-closing-parenthesis-handler #f)
				      (current-dot-handler #f))
			 (let loop ((syntax* '()))
			   (current-closing-parenthesis-handler
			    (lambda ()
			      (return (reverse syntax*))))
			   (current-dot-handler
			    (lambda ()
			      (k syntax*)))
			   (loop (cons (parameterize
					   ((start #f))
					 (read-syntax))
				       syntax*)))))))
		   (rest (parameterize ((start #f)) (read-syntax)))
		   (datum (unwrap-syntax rest))
		   (list (append-reverse syntax*
					 (if (or (pair? datum) (null? datum))
					     datum
					     rest))))
		(parameterize
		    ((current-closing-parenthesis-handler
		      (lambda ()
			(return list))))
		  (parameterize ((start #f)) (read-syntax))
		  (reader-error "expected end of list after dot")
		  (return (syntax list))))))))))
     
     (define (read-abbreviation identifier)
       (let ((head (syntax identifier)))
	 (syntax (list head
		       (parameterize ((start #f)) (read-syntax))))))

     (define labels (imap label-comparator))

     (define (read-label)
       (let ((number (read-number)))
	 (case (read)
	   ((#\=)
	    (cond
	     ((imap-ref/default labels number #f)
	      (reader-error "duplicate label")
	      #f)
	     (else
	      (let ((label (make-label)))
		(set! labels (imap-replace labels number label))
		(and-let* ((referenced-syntax (read-syntax)))
		  (label-set-syntax! label referenced-syntax)
		  (cond
		   ((syntax-reference referenced-syntax)
		    ;; The referenced systax is a reference itself.
		    => (lambda (referenced-label)
			 (cond
			  ((eq? referenced-label label)
			   (reader-error "invalid self-reference")
			   #f)
			  (else
			   (label-add-references! referenced-label
						  (label-references label))))))
		   (else
		    ;; The references can be patched
		    (let ((datum (unwrap-syntax referenced-syntax)))
		      (label-for-each-reference
		       (lambda (reference)
			 (syntax-set-datum! reference datum)
			 (syntax-set-reference! reference referenced-syntax))
		       label))
		    (label-delete-references! label)))
		  referenced-syntax)))))
	   ((#\#)
	    (and-let*
		((label
		  (imap-ref labels number (lambda ()
					    (reader-error "unknown reference")
					    #f))))
	      (or (and-let* ((referenced-syntax (label-syntax label)))
		    (syntax (unwrap-syntax referenced-syntax) referenced-syntax))
		  (let ((referencing-syntax (syntax label)))
		    (label-add-reference! label referencing-syntax)
		    referencing-syntax))))
	   (else
	    (reader-error "expected ‘#’ or ‘=’ after label")
	    #f))))
     
     (define (read-syntax)
       (start (position))
       (cond
	((eof-object? (peek))
	 (read))
	;; Numbers
	((char<=? #\0 (peek) #\9)
	 (or (number (read-token))
	     (read-syntax)))
	;; Identifiers
	((initial? (peek))
	 (or (read-identifier)
	     (read-syntax)))
	;; Peculiar identifiers
	((or (sign? (peek))
	     (char=? (peek) #\.))
	 (or (read-peculiar-identifier)
	     (read-syntax)))
	(else
	 (case (read)
	   ;; Skip whitespace
	   ((#\newline #\return #\space #\tab)
	    (read-syntax))
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
	    (read-syntax))
	   ;; Strings
	   ((#\")
	    (read-string))
	   ;; Identifiers enclosed in vertical lines
	   ((#\|)
	    (read-symbol))
	   ;; Lists
	   ((#\()
	    (or (read-list)
		(read-syntax)))
	   ;; Closing parenthesis
	   ((#\))
	    (begin ((current-closing-parenthesis-handler))
		   (read-syntax)))
	   ;; Quote
	   ((#\')
	    (read-abbreviation (symbol->identifier 'quote)))
	   ;; Quasiquote
	   ((#\`)
	    (read-abbreviation (symbol->identifier 'quasiquote)))
	   ;; Unquote
	   ((#\,)
	    (cond
	     ((char=? (peek) #\@)
	      (read)
	      (read-abbreviation (symbol->identifier 'unquote-splicing)))
	     (else
	      (read-abbreviation (symbol->identifier 'unquote)))))
	   ;; Sharp syntax
	   ((#\#)
	    (or
	     (with-eof-handler
	      (lambda ()
		(reader-error "incomplete sharp syntax at end of input"))
	      (lambda ()
		(let ((char (char-foldcase (peek))))
		  (cond
		   ((member char '(#\t #\f) char=?)
		    (read-boolean))
		   ((char<=? #\0 char #\9)
		    (read-label))
		   (else
		    (read)		
		    (case char
		      ;; Numbers
		      ((#\e #\i #\b #\o #\d #\x)
		       => (lambda (char)
			    (number (string-append "#"
						   (string char) 
						   (read-token)))))
		      ;; Nested comment
		      ((#\|)
		       (read-nested-comment)
		       #f)
		      ;; Datum comment
		      ((#\;)
		       (with-eof-handler
			(lambda ()
			  (reader-error "incomplete datum comment"))
			(lambda ()
			  (parameterize ((start #f)) (read-syntax))))
		       #f)
		      ;; Directives
		      ((#\!)
		       (read-directive)
		       #f)
		      ;; Characters
		      ((#\\)
		       (read-character))
		      ;; Vector
		      ((#\()
		       (read-vector))
		      ;; Bytevector
		      ((#\u)
		       (read-bytevector))
		      (else
		       => (lambda (char)
			    (reader-error "invalid sharp syntax ‘#~a’" char)
			    #f))))))))
	     (read-syntax)))
	   ;; Invalid character
	   (else
	    => (lambda (char)
		 (reader-error "unexpected character ‘~a’ in input" char)
		 (read-syntax)))))))

     (with-eof-handler
      (lambda () (return #f))
      read-syntax))))

(define (read-file source ci? context)
  (make-coroutine-generator
   (lambda (yield)
     (call-with-current-continuation
      (lambda (exit)
	(with-exception-handler
	 (lambda (condition)
	   (cond
	    ((file-error? condition)
	     (raise-syntax-error context "file error occured with file ‘~a’" source)
	     (exit))
	    (else
	     (raise-continuable condition))))
	(lambda ()
	  (call-with-input-file source
	    (lambda (port)
	      (let ((source-port (make-source-port port source ci?)))
		(let loop ()
		  (and-let* ((syntax (read-syntax source-port context)))
		    (yield syntax)
		    (loop)))))))))))))
