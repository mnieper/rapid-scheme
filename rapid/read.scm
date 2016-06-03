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
     
     (define (read-escape)
       ;; TODO: line endings, hex sequences  
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
	 (else
	  => (lambda (char)
	       (reader-error "invalid escape ‘~a’" char)
	       #f))))
     
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
		  (else
		   => (lambda (char)
			(cons char (loop))))))))))))

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
		 ((#\newline))
		 ((#\return)
		  (when (char=? (peek) #\newline)
		    (read)))
		 (else
		  (loop))))
	     (loop))
	    ;; Strings
	    ((#\")
	     (read-string))
	    ;; Sharp syntax
	    ((#\#)
	     (with-eof-handler
	      (lambda ()
		(reader-error "incomplete sharp syntax at end of input"))
	      (lambda ()
		(case (read)
		  ;; Nested comment
		  ((#\|)
		   (read-nested-comment)
		   (loop))
		  ;; Datum comment
		  ((#\;)
		   (parameterize ((start #f)) (loop))
		   (loop))
		  (else
		   => (lambda (char)
			(reader-error "invalid sharp syntax ‘#~a’" char)
			(loop)))))))
	    ;; Invalid character
	    (else
	     => (lambda (char)
		  (reader-error "unexpected character ‘~a’ in input" char)
		  (loop))))))))))
