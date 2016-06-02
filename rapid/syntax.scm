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

(define-record-type <source-location>
  (make-source-location source start end)
  source-location?
  (source source-location-source)
  (start source-location-start)
  (end source-location-end))

(define (make-position line column) (vector line column))
(define (position-line position) (vector-ref position 0))
(define (position-column position) (vector-ref position 1))
(define (source-location-start-line source-location)
  (position-line (source-location-start source-location)))
(define (source-location-start-column source-location)
  (position-column (source-location-start source-location)))
(define (source-location-end-line source-location)
  (position-line (source-location-end source-location)))
(define (source-location-end-column source-location)
  (position-column (source-location-end source-location)))

(define-record-type <syntax>
  (make-syntax datum source-location context reference)
  syntax?
  (datum syntax-datum syntax-set-datum!)
  (source-location syntax-source-location)
  (context syntax-context syntax-set-context!)
  (reference syntax-reference)
  (aux syntax-aux syntax-set-aux!))

(define syntax->datum
  (case-lambda
   ((syntax) (syntax->datum syntax (lambda (datum) datum)))
   ((syntax converter)
    (let syntax->datum ((syntax syntax))
      (cond
       ((syntax-reference syntax) => syntax-aux)
       (else
	(let ((datum (syntax-datum syntax)))
	  (cond
	   ((vector? datum)
	    (let* ((n (vector-length datum))
		   (vector (make-vector n)))
	      (syntax-set-aux! syntax vector)
	      (do ((i 0 (+ i 1)))
		  ((>= i n))
		(vector-set! vector i (syntax->datum (vector-ref datum i))))
	      vector))
	   ((pair? datum)
	    (let ((pair (list #f)))
	      (syntax-set-aux! pair)
	      (set-car! pair (syntax->datum (car datum)))
	      (do ((datum datum (cdr datum))
		   (pair pair (cdr pair)))
		  ((not (pair? (cdr datum)))
		   (unless (null? (cdr datum))
		     (set-cdr! pair (syntax->datum (cdr datum)))))
		(set-cdr! pair (list (syntax->datum (cadr datum)))))
	      pair))
	   (else
	    (converter datum))))))))))

(define error-message-count (make-parameter 0))

(define-record-type <syntax-exception>
  #f
  syntax-exception?
  (syntax syntax-exception-syntax)
  (message syntax-exception-message))

(define-record-type (<syntax-note> <syntax-exception>)
  (make-syntax-note syntax message)
  syntax-note?)

(define-record-type (<syntax-warning> <syntax-exception>)
  (make-syntax-warning syntax message)
  syntax-warning?)

(define-record-type (<syntax-error> <syntax-exception>)
  (make-syntax-error syntax message)
  syntax-error?)

(define-record-type (<syntax-fatal-error> <syntax-exception>)
  (make-syntax-fatal-error syntax message)
  syntax-fatal-error?)

(define (syntax-exception-name exception)
  (cond
   ((syntax-note? exception) "note")
   ((syntax-warning? exception) "warning")
   ((syntax-error? exception) "error")
   ((syntax-fatal-error? exception "fatal error"))
   (else (error "not a syntax exception" exception))))

(define (syntax-note syntax message . object*)
  (raise-continuable (make-syntax-note syntax
				       (apply format message object*))))
(define (syntax-warning syntax message . object*)
  (raise-continuable (make-syntax-warning syntax
					  (apply format message object*))))
(define (syntax-error syntax message . object*)
  (error-message-count (+ (error-message-count) 1))
  (raise-continuable (make-syntax-error syntax
					(apply format message object*))))
(define (syntax-fatal-error syntax message . object*)
  (raise (make-syntax-fatal-error syntax
				  (apply format message object*))))

(define (print-internal-error exception)
  (flush-output (current-output-port))
  (print-program-name)
  (write-string "internal error" (current-error-port))
  (newline (current-error-port)))

(define (print-exception exception)
  (flush-output (current-output-port))
  (print-source-location (exception-syntax exception))
  (write-string (exception-name exception) (current-error-port))
  (write-string ": " (current-error-port))
  (write-string (syntax-exception-message exception) (current-error-port))
  ;; TODO: Print context.
  (newline (current-error-port)))

(define (print-source-location syntax)
  (cond
   ((syntax-source-location syntax)
    => (lambda (source-location)
	 (write-string (source-location-source source-location)
		       (current-error-port))
	 (write-string ": " (current-error-port))
	 (let
	     ((start-line (source-location-start-line source-location))
	      (end-line (source-location-end-line source-location))
	      (start-column (source-location-start-column source-location))
	      (end-column (source-location-end-column source-location)))
	   (display start-line (current-error-port))
	   (write-string "." (current-error-port))
	   (display (+ start-column 1) (current-error-port))
	   (write-string "-" (current-error-port))
	   (unless (= start-line end-line)
	     (display end-line (current-error-port))
	     (write-string "." (current-error-port)))
	   (display end-column (current-error-port))
	   (write-string ": " (current-error-port)))))
   (else
    (print-program-name))))

(define (print-program-name)
  (write-string (car (command-line)) (current-error-port))
  (write-string ": " (current-error-port)))

(define-syntax syntax-exception-guard 
  (syntax-rules ()
    ((_ . body)
     (parameterize ((error-message-count 0))
       (guard (condition
	       ((syntax-exception? condition)
		(print-exception condition)
		(when (syntax-fatal-error? condition)
		  (exit #f)))
	       (else
		(print-internal-error)
		(raise condition)))
	 . body)))))
