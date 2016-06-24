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

;;; Source locations

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
  (datum unwrap-syntax syntax-set-datum!)
  (source-location syntax-source-location)
  (context syntax-context syntax-set-context!)
  (reference syntax-reference syntax-set-reference!)
  (aux syntax-aux syntax-set-aux!))

(define (syntax->datum syntax)
  (cond
   ((syntax-reference syntax) => syntax-aux)
   (else
    (let ((datum (unwrap-syntax syntax)))
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
	  (syntax-set-aux! syntax pair)
	  (set-car! pair (syntax->datum (car datum)))
	  (do ((datum datum (cdr datum))
	       (pair pair (cdr pair)))
	      ((not (pair? (cdr datum)))
	       (unless (null? (cdr datum))
		 (set-cdr! pair (syntax->datum (cdr datum)))))
	    (set-cdr! pair (list (syntax->datum (cadr datum)))))
	  pair))
       ((identifier? datum)
	(identifier->symbol datum))
       (else
	datum))))))

(define derive-syntax
  (case-lambda
   ((datum)
    (derive-syntax datum #f #f))
   ((datum syntax)
    (derive-syntax datum syntax (if syntax (syntax-context syntax) #f)))
   ((datum syntax context)
    (let ((location (if syntax (syntax-source-location syntax) #f)))
      (let loop ((datum datum))
	(cond
	 ;; XXX: Does not handle vectors or improper lists
	 ((syntax? datum)
	  (make-syntax (unwrap-syntax datum)
		       (syntax-source-location datum)
		       context
		       #f))
	 ((list? datum)
	  (make-syntax (map loop datum) location context #f))
	 ((symbol? datum)
	  (make-syntax (symbol->identifier datum) location context #f))
	 (else
	  (make-syntax datum location context #f))))))))

;; Syntax messages

(define current-log-level (make-parameter 'warning))

(define error-message-count (make-parameter 0))

(define-record-type <syntax-exception>
  #f
  syntax-exception?
  (exception-syntax syntax-exception-syntax)
  (message syntax-exception-message))

(define-record-type (<syntax-note> <syntax-exception>)
  (make-syntax-note exception-syntax message)
  syntax-note?)

(define-record-type (<syntax-info> <syntax-exception>)
  (make-syntax-info exception-syntax message)
  syntax-info?)

(define-record-type (<syntax-warning> <syntax-exception>)
  (make-syntax-warning exception-syntax message)
  syntax-warning?)

(define-record-type (<syntax-error> <syntax-exception>)
  (make-syntax-error exception-syntax message)
  syntax-error?)

(define-record-type (<syntax-fatal-error> <syntax-exception>)
  (make-syntax-fatal-error exception-syntax message)
  syntax-fatal-error?)

(define (syntax-exception-name exception)
  (cond
   ((syntax-info? exception) "info")
   ((syntax-note? exception) "note")
   ((syntax-warning? exception) "warning")
   ((syntax-error? exception) "error")
   ((syntax-fatal-error? exception) "fatal error")
   (else (error "not a syntax exception" exception))))

(define (raise-syntax-info syntax message . object*)
  (raise-continuable (make-syntax-info syntax
				       (apply format message object*))))
(define (raise-syntax-note syntax message . object*)
  (raise-continuable (make-syntax-note syntax
				       (apply format message object*))))
(define (raise-syntax-warning syntax message . object*)
  (raise-continuable (make-syntax-warning syntax
					  (apply format message object*))))
(define (raise-syntax-error syntax message . object*)
  (error-message-count (+ (error-message-count) 1))
  (raise-continuable (make-syntax-error syntax
					(apply format message object*))))
(define (raise-syntax-fatal-error syntax message . object*)
  (raise (make-syntax-fatal-error syntax
				  (apply format message object*))))

(define (print-internal-error)
  (flush-output-port (current-output-port))
  (print-program-name)
  (write-string "internal error" (current-error-port))
  (newline (current-error-port)))

(define (print-exception exception)
  (flush-output-port (current-output-port))
  (print-source-location (syntax-exception-syntax exception))
  (write-string (syntax-exception-name exception) (current-error-port))
  (write-string ": " (current-error-port))
  (write-string (syntax-exception-message exception) (current-error-port))
  (newline (current-error-port))
  (print-context (syntax-exception-syntax exception)))

(define (print-context syntax)
  (and-let* ((syntax)
	     (context (syntax-context syntax)))
    (when (syntax-source-location context)
      (print-source-location context)
      (write-string "  included from here" (current-error-port))
      (newline (current-error-port)))
    (print-context context)))

(define (print-source-location syntax)
  (cond
   ((and syntax (syntax-source-location syntax))
    => (lambda (source-location)
	 (write-string (source-location-source source-location)
		       (current-error-port))
	 (write-string ": " (current-error-port))
	 (let
	     ((start-line (source-location-start-line source-location))
	      (end-line (source-location-end-line source-location))
	      (start-column (+ (source-location-start-column source-location) 1))
	      (end-column (source-location-end-column source-location)))
	   (display start-line (current-error-port))
	   (write-string "." (current-error-port))
	   (display start-column (current-error-port))
	   (unless (and (= start-line end-line) (= start-column end-column))
	     (write-string "-" (current-error-port))
	     (unless (= start-line end-line)
	       (display end-line (current-error-port))
	       (write-string "." (current-error-port)))
	     (display end-column (current-error-port)))
	   (write-string ": " (current-error-port)))))
   (else
    (print-program-name))))

(define (print-program-name)
  (cond
   ((command-line)
    => (lambda (command-line)
	 (write-string (car command-line) (current-error-port))
	 (write-string ": " (current-error-port))))))

(define (with-syntax-exception-handler thunk)
  (with-exception-handler
   (lambda (condition)
     (cond
      ((and (syntax-exception? condition)
	    (or (not (syntax-info? condition)) (eq? (current-log-level) 'info)))
       (print-exception condition)
       (when (syntax-fatal-error? condition)
	 (exit #f))
       #f)
      (else
       (print-internal-error)
       (raise condition))))
   thunk))

(define (with-syntax-exception-guard thunk)
  (with-exception-handler
   (lambda (condition)
     (cond
      ((syntax-error? condition)
       #f)
      ((syntax-fatal-error? condition)
       (print-exception condition)
       (exit #f))
      (else
       #f)))
   thunk))
