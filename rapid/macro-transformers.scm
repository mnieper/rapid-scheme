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

(define (make-er-macro-transformer transformer)
  (let ((macro-environment (current-syntactic-environment)))
    (lambda (syntax)
      (let ((environment (current-syntactic-environment)))      
      
	(define renames (imap identifier-comparator))
	
	(define (rename identifier)
	  (cond
	   ((imap-ref renames identifier #f))
	   (else
	    (let ((renamed-identifier (close-syntax identifier macro-environment)))
	      (set! renames (imap-replace identifier renamed-identifier))
	      renamed-identifier))))

	(define (compare identifier1 identifier2)
	  (identifier=? environment identifier1 environment identifier2))

	(transformer syntax rename compare)))))

(define (make-syntax-rules-transformer
	 ellipsis? literal? underscore? syntax-rule-syntax* transformer-syntax)

  (define pattern-syntax-vector
    (list->vector
     (map
      (lambda (syntax-rule-syntax)
	(let ((pattern-syntax (car (unwrap-syntax syntax-rule-syntax))))
	  (derive-syntax (cdr (unwrap-syntax pattern-syntax)) pattern-syntax)))
      syntax-rule-syntax*)))

  (define template-syntax-vector
    (list->vector
     (map
      (lambda (syntax-rule-syntax)
	(cadr (unwrap-syntax syntax-rule-syntax)))
      syntax-rule-syntax*)))

  (define (compile-pattern pattern-syntax rule-index)
    (and-let*
	((pattern (unwrap-syntax pattern-syntax))
	 ((or (and (pair? pattern) (identifier? (unwrap-syntax (car pattern))))
	      (raise-syntax-error pattern-syntax
				  "invalid pattern"))))
      (receive (identifiers matcher)
	  (compile-list-pattern (derive-syntax (cdr pattern) pattern-syntax))
	(and matcher
	     (vector identifiers
		     (lambda (syntax)
		       (matcher
			(derive-syntax (cdr (unwrap-syntax syntax)) syntax)
			(vector-ref pattern-syntax-vector rule-index))))))))

  (define (compile-template template-syntax variable-map rule-index)
    ;; XXX: Slots is a vector of indices of the matched variables in the
    ;; pattern variables.
    ;; FIXME: Make this clearer
    (receive (slots transcriber)
	(compile-subtemplate template-syntax variable-map 0)
      (and transcriber
	   (lambda (pattern-variables)
	     (transcriber
	      (vector-map
	       (lambda (slot)
		 (vector-ref pattern-variables slot))
	       slots)
	      (vector-ref template-syntax-vector rule-index))))))
       
  (define rules
    (let loop ((syntax-rule-syntax* syntax-rule-syntax*)
	       (i 0))
      (cond
       ((null? syntax-rule-syntax*)
	'())
       ((and-let*
	    ((syntax-rule-syntax (car syntax-rule-syntax*))
	     (syntax-rule (unwrap-syntax syntax-rule-syntax))
	     ((or (and (list? syntax-rule) (= (length syntax-rule) 2))
		  (raise-syntax-error syntax-rule-syntax
				      "bad syntax rule")))
	     (variable-map+matcher
	      (compile-pattern (car syntax-rule) i))
	     (transcriber
	      (compile-template (cadr syntax-rule)
				(vector-ref variable-map+matcher 0)
				i)))
	  (vector (vector-ref variable-map+matcher 1)
		  transcriber))
	=> (lambda (matcher+transcriber)
	     (let ((rule (make-rule (vector-ref matcher+transcriber 0)
				    (vector-ref matcher+transcriber 1))))
	       (cons rule (loop (cdr syntax-rule-syntax*)
				(+ i 1))))))
       (else
	(loop (cdr syntax-rule-syntax*) (+ i 1))))))

  (make-er-macro-transformer
   (lambda (syntax rename compare)
     (let loop ((rules rules))
       (cond
	((null? rules)
	 (raise-syntax-error syntax "no expansion for macro use")
	 (raise-syntax-note transformer-syntax
			    "the macro definition was here")
	 #f)
	((rule-match (car rules) syntax)
	 => (lambda (match)
	      (rule-transcribe (car rules) match)))
	(else
	 (loop (cdr rules))))))))

(define (make-rule matcher transcriber)
  (vector matcher transcriber))
(define (rule-match rule syntax)
  ((vector-ref rule 0) syntax))
(define (rule-transcribe rule match)
  ((vector-ref rule 1) match))

(define (compile-list-pattern pattern-syntax)
  ;; FIXME
  (values #f #f))

(define (compile-subtemplate template-syntax variable-map depth)
  ;; FIXME
  (values #f #f))
