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

  (define rules
    (let loop ((syntax-rule-syntax* syntax-rule-syntax*))
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
	      (compile-pattern (car syntax-rule)))
	     (transcriber
	      (compile-template (cadr syntax-rule)
				(vector-ref variable-map+matcher 0))))
	  (vector (vector-ref variable-map+matcher 1)
		  transcriber))
	=> (lambda (matcher+transcriber)
	     (let ((rule (make-rule (vector-ref matcher+transcriber 0)
				    (vector-ref matcher+transcriber 1))))
	       (cons rule (loop (cdr syntax-rule-syntax*))))))
       (else
	(loop (cdr syntax-rule-syntax*))))))

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

(define (compile-pattern pattern-syntax)
  (and-let*
      ((pattern (unwrap-syntax pattern-syntax))
       ((or (and (pair? pattern) (identifier? (unwrap-syntax (car pattern))))
	    (raise-syntax-error pattern-syntax
				"invalid pattern")))
       (*pattern-syntax
	(derive-syntax (cdr pattern) pattern-syntax)))
    (receive (identifiers matcher)
	(compile-list-pattern *pattern-syntax)
      (and matcher
	   (vector identifiers
		   (lambda (syntax)
		     (matcher
		      (derive-syntax (cdr (unwrap-syntax syntax)) syntax)
		      *pattern-syntax)))))))

(define (compile-template template-syntax variable-map)
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
	    template-syntax)))))

(define (compile-list-pattern pattern-syntax)
  ;; FIXME
  (values #f #f))

(define (compile-subtemplate template-syntax variable-map depth)
  ;; FIXME
  (values #f #f))

;;; XXX
;;; Some notes on the algorithm employed here:
;;;
;;; At macro-definition time, the syntax rules consisting of a pattern
;;; and a template are each compiled into a matcher and a transcriber
;;; that are invoked. When the macro is being used, the matchers and
;;; transcribers are called.  The input of the pattern compiler is the
;;; pattern syntax. The output is a vector consisting of a variable
;;; map and a matcher procedure.

;;; The variable map is a map that maps an identifier pattern to a
;;; pattern variable. A pattern variable is a structure consisting of
;;; three fields: an index, its depth, and the corresponding syntax.
;;;
;;; The matcher procedure takes the syntax to match and evaluates
;;; either to #f in case the syntax does not match the pattern or to a
;;; match. A match is a vector consisting of the matched unwrapped syntax.

;;; The input of the template compiler is the variable map of the
;;; corresponding pattern compiler evaluation and the template
;;; syntax. The output of the template compiler is a vector consisting
;;; of a slots of indices and a transcriber procedure.

;;; The slot vector models a mapping from the indices of the matched
;;; variables actually used by the transcriber to the indices of the
;;; matched variables returned by the match procedure. The transcriber
;;; procedure takes the subvector of the pattern variables it needs
;;; and the template syntax.
