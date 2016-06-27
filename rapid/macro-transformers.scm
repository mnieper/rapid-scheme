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

;;; Parameters

(define current-rename (make-parameter #f))
(define current-compare (make-parameter #f))
(define (rename identifier)
  ((current-rename) identifier))
(define (compare identifier1 identifier2)
  ((current-compare) identifier1 identifier2))

;;; Macro transformers

(define (make-er-macro-transformer transformer)
  (let ((macro-environment (current-syntactic-environment)))
    (lambda (syntax)
      (let ((environment (current-syntactic-environment)))      
      
	(define renames (imap identifier-comparator))
	
	(define (rename identifier)
	  (cond
	   ((imap-ref/default renames identifier #f))
	   (else
	    (let ((renamed-identifier (close-syntax identifier macro-environment)))
	      (set! renames (imap-replace renames identifier renamed-identifier))
	      renamed-identifier))))

	(define (compare identifier1 identifier2)
	  (identifier=? environment identifier1 environment identifier2))

	(transformer syntax rename compare)))))

(define (make-syntax-rules-transformer
	 %ellipsis? literal? underscore? syntax-rule-syntax* transformer-syntax)

  (define ellipsis-active? (make-parameter #t))
  
  (define (ellipsis? form)
    (and (ellipsis-active?)
	 (identifier? form)
	 (%ellipsis? form)))
  
  ;; Helper functions for the pattern compilers

  (define (compile-pattern pattern-syntax)
    (and-let*
	((pattern (unwrap-syntax pattern-syntax))
	 ((or (and (pair? pattern) (identifier? (unwrap-syntax (car pattern))))
	      (raise-syntax-error pattern-syntax
				  "invalid pattern")))
	 (*pattern-syntax
	  (derive-syntax (cdr pattern) pattern-syntax)))
      (receive (variable-map matcher)
	  (compile-subpattern *pattern-syntax)
	(and matcher
	     (vector variable-map
		     (lambda (syntax)
		       (matcher
			(derive-syntax (cdr (unwrap-syntax syntax)) syntax))))))))

  (define (compile-subpattern pattern-syntax)
    (let ((pattern (unwrap-syntax pattern-syntax)))
      (cond
       ((identifier? pattern)
	(cond
	 ;; Literal identifier
	 ((literal? pattern)
	  (values (make-pattern-variable-map)
		  (lambda (syntax)
		    (and-let*
			((datum (unwrap-syntax syntax))
			 ((identifier? datum))
			 ((compare (unwrap-syntax syntax)
				   (rename (unwrap-syntax pattern-syntax)))
			 #()))))))
	 ;; _ identifier
	 ((underscore? pattern)
	  (values (make-pattern-variable-map)
		  (lambda (syntax)
		    #())))
	 ;; Pattern variable
	 (else
	  (values (imap-replace (make-pattern-variable-map)
				pattern
				(make-pattern-variable 0 0 pattern-syntax))
		  (lambda (syntax)
		    (vector (unwrap-syntax syntax)))))))
       ;; Vector
       ((vector? pattern)
	(receive (variables-map matcher)
	    (compile-list-pattern (derive-syntax (vector->list pattern)
						 pattern-syntax))
	  (if variables-map	    
	      (values variables-map
		      (lambda (syntax)
			(let ((datum (unwrap-syntax syntax)))
			  (and-let*
			      (((vector? datum))
			       (list (vector->list datum)))
			    (matcher (derive-syntax list
						    syntax)
				     (derive-syntax (vector->list pattern-syntax)
						    pattern-syntax))))))
	      (values #f #f))))
       ;; Circular list
       ((circular-list? pattern)
	(raise-syntax-error pattern-syntax "circular pattern in source")
	(values #f #f))
       ;; Empty list
       ((null? pattern)
	(values (make-pattern-variable-map)
		(lambda (syntax)
		  (and (null? (unwrap-syntax syntax))
		       #()))))
       ;; Finite list
       ((pair? pattern)
	(compile-list-pattern pattern-syntax))
       ((constant? pattern)
	(values (make-pattern-variable-map)
		(lambda (syntax)
		  (and (equal? (unwrap-syntax syntax)
			       (unwrap-syntax pattern-syntax))
		       #()))))
       (else
	(raise-syntax-error pattern-syntax "invalid subpattern")
	(values #f #f)))))
    
  (define (compile-list-pattern pattern-syntax)
    
    (define variable-count 0)
    (define variable-map (make-pattern-variable-map))
    
    (define (insert-pattern-variable! identifier variable offset depth-increase)
      (cond
       ((imap-ref/default variable-map identifier #f)
	=> (lambda (previous-variable)
	     (raise-syntax-error (pattern-variable-syntax variable)
				 "pattern variable has already appeared once")
	     (raise-syntax-note (pattern-variable-syntax previous-variable)
				"previous appearance was here")))
       (else
	(set! variable-map
	      (imap-replace variable-map
			    identifier
			    (make-pattern-variable (+ offset
						      (pattern-variable-index variable))
						   (+ depth-increase
						      (pattern-variable-depth variable))
						   (pattern-variable-syntax variable))))))
      (set! variable-count (+ variable-count 1)))
  
    (define (submatcher-compile! pattern-element)
      (let*-values
	  (((depth-increase)
	    (if (pattern-element-repeated? pattern-element) 1 0))
	   ((subvariable-map matcher)
	    (compile-subpattern (pattern-element-syntax pattern-element)))
	   ((offset)
	    variable-count)
	   ((submatcher)
	    (make-submatcher pattern-element subvariable-map matcher offset)))
	(when subvariable-map	
	  (imap-for-each
	   (lambda (identifier variable)
	     (insert-pattern-variable! identifier variable offset depth-increase))
	   subvariable-map))
	submatcher))
    
    (define-values (pattern-elements repeated? dotted-pattern?)
      (analyze-pattern-list (unwrap-syntax pattern-syntax)))

    (define pattern
      (unwrap-syntax pattern-syntax))

    (define (make-submatcher pattern-element variable-map matcher offset)
      (let ((element-index (pattern-element-index pattern-element))
	    (from-end? (pattern-element-from-end? pattern-element))
	    (element-repeated? (pattern-element-repeated? pattern-element)))
	(lambda (input-length input match)
	  (let*
	      ((input-index
		(if from-end?
		    (+ input-length (- element-index (length pattern-elements)
				       1
				       (if (and dotted-pattern? repeated?) -1 0)))
		    element-index)))
	    (if element-repeated?
		;; Repeated element
		(and-let*
		    ((input-end
		      (+ input-length (- input-index
					 (length pattern-elements)
					 (if dotted-pattern? -1 0))))
		     (submatch*
		      (unfold (lambda (index) (> index input-end))
			      (lambda (index)
				(matcher (vector-ref input index)))
			      (lambda (index) (+ index 1))
			      input-index))
		     ((every (lambda (submatch) submatch) submatch*)))
		  (imap-for-each
		   (lambda (identifier variable)
		     (vector-set! match
				  (+ offset (pattern-variable-index variable))
				  (map (lambda (submatch)
					 (vector-ref submatch (pattern-variable-index variable)))
				       submatch*)))
		   variable-map)
		  #t)
		;; Non-repeated element
		(and-let*
		    ((submatch (matcher (vector-ref input input-index))))
		  (imap-for-each
		   (lambda (identifier variable)
		     (vector-set! match
				  (+ offset (pattern-variable-index variable))
				  (vector-ref submatch
					      (pattern-variable-index variable))))
		   variable-map)
		  #t))))))
    
    (define submatchers (map-in-order submatcher-compile! pattern-elements))

    (define (matcher syntax)
      (and-let*
	  ((form
	    (unwrap-syntax syntax))
	   ((or (not (circular-list? form))
		(raise-syntax-error "circular list in source" syntax)))
	   (left
	    (drop-right form 0))
	   (right
	    (take-right form 0))
	   (input-length
	    (length left))
	   ((or dotted-pattern? (null? right)))
	   ((if (or repeated? dotted-pattern?)
		(>= input-length (- (length pattern-elements)
				    (if repeated? 1 0)
				    (if dotted-pattern? 1 0)))
		(= input-length (length pattern-elements))))
	   (input
	    (list->vector (if dotted-pattern?
			      (if repeated?
				  (append left (list (if (null? right)
							 (derive-syntax '() syntax)
							 right)))
				  (receive (head tail)
				      (split-at left (- (length pattern-elements) 1))
				    (let ((tail (append tail right)))
				      (append head (list (if (syntax? tail)
							     tail
							     (derive-syntax tail syntax)))))))
			      left)))
	   (match (make-vector variable-count))
	   ((every (lambda (submatcher)
		     (submatcher input-length input match))
		   submatchers)))
	match))
    
    (values variable-map
	    matcher))
  
  ;; Takes a finite list of patterns. Returns three values. The first
  ;; value is a list of pattern elements, the second value is a boolean
  ;; saying whether an ellipsis in the pattern an the last value is a
  ;; boolean saying whether the list of patterns is an improper list.
  ;; Here, a pattern element is a vector consisting of four entries. The
  ;; first entry is the syntax of the element, the second one the index,
  ;; the third one is a boolean specifying whether to count the index
  ;; from the front or the back and the last entry is a boolean
  ;; specifying whether the entry itself is repeated.
  
  (define (analyze-pattern-list pattern-list)
    
    (define (return reversed-elements repeated-element dotted?)
      (values (reverse (if repeated-element
			   (cons repeated-element reversed-elements)
			   reversed-elements))
	      (and repeated-element #t)
	      dotted?))
    
    (let loop ((pattern-list pattern-list)
	       (reversed-elements '())
	       (repeated-element #f)
	       (i 0))
      (cond
       ((null? pattern-list)
	(return reversed-elements repeated-element #f))
       ((pair? pattern-list)
	(cond
	 ((ellipsis? (unwrap-syntax (car pattern-list)))
	  (cond
	   (repeated-element
	    (raise-syntax-error (car pattern-list) "extraneous ellipsis")
	    (loop (cdr pattern-list)
		  reversed-elements
		  repeated-element
		  i))
	   ((null? reversed-elements)
	    (raise-syntax-error (car pattern-list)
				"ellipsis not preceded by a pattern")
	    (loop (cdr pattern-list)
		  reversed-elements
		  repeated-element
		  i))
	   (else
	    (pattern-element-set-repeated?! (car reversed-elements) #t)
	    (loop (cdr pattern-list)
		  (cdr reversed-elements)
		  (car reversed-elements)
		  (+ i 1)))))
	 (else
	  (loop (cdr pattern-list)
		(cons (make-pattern-element (car pattern-list)
					    i
					    (and repeated-element #t)
					    #f)
		      reversed-elements)
		repeated-element (+ i 1)))))
       (else
	(cond
	 ((ellipsis? (unwrap-syntax pattern-list))
	  (raise-syntax-error pattern-list "ellipsis not allowed as dotted tail")
	  (return reversed-elements repeated-element #f))
	 (else
	  (return (cons (make-pattern-element pattern-list
					      i
					      (and repeated-element #t)
					      #f)
			reversed-elements)
		  repeated-element
		  #t)))))))

  ;; Template compiler

  (define current-context (make-parameter #f))
  
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
	      pattern-variables)))))

  (define (compile-subtemplate template-syntax variable-map depth)
    ;; FIXME: We don't get the contexts right in all cases. Do some experiments.
    (let ((template (unwrap-syntax template-syntax)))
      (cond
       ((identifier? template)
	(cond
	 ((ellipsis? template)
	  (raise-syntax-error template-syntax "extraneous ellipsis in template")
	  (values #f #f))
	 ((imap-ref/default variable-map template #f)
	  => (lambda (variable)
	       (let ((variable-depth (pattern-variable-depth variable)))
		 (if (zero? variable-depth)
		     (values #()
			     (lambda (match pattern-variables)
			       (derive-syntax (vector-ref
					       pattern-variables
					       (pattern-variable-index variable))
					      template-syntax
					      (current-context))))
		     (cond
		      ((> variable-depth depth)
		       (raise-syntax-error template-syntax
					   "pattern variable followed by too few ellipses")
		       (values #f #f))
		      ((< variable-depth depth)
		       (raise-syntax-error template-syntax
					   "pattern variable followed by too many ellipses"))
		      (else
		       (values (vector (pattern-variable-index variable))
			       (lambda (match pattern-variables)
				 (derive-syntax (vector-ref match 0)
						template-syntax
						(current-context))))))))))
	 (else
	  (values #()
		  (lambda (match pattern-variables)
		    (derive-syntax (rename template) template-syntax (current-context)))))))
       ((circular-list? template)
	(raise-syntax-error "circular template in source" template-syntax)
	(values #f #f))
       ((null? template)
	(values #()
		(lambda (match pattern-variables)
		  (derive-syntax '() template-syntax (current-context)))))
       ((pair? template)
	(if (and (list? template)
		 (= (length template) 2)
		 (ellipsis? (unwrap-syntax (car template))))
	    (parameterize
		((ellipsis-active? #f))
	      (compile-subtemplate (cadr template) variable-map depth))
	    (compile-list-template template-syntax variable-map depth)))
       ((vector? template)
	(receive (slots transcriber)
	    (compile-list-template (derive-syntax (vector->list template) template-syntax)
				   variable-map
				   depth)
	  (if transcriber
	      (values slots
		      (lambda (match pattern-variables)
			(let ((output-syntax (transcriber match pattern-variables)))
			  (derive-syntax (list->vector (unwrap-syntax output-syntax))
					 output-syntax))))
	      (values #f #f))))	  
       ((constant? template)
	(values #()
		(lambda (match pattern-variables)
		  (derive-syntax (unwrap-syntax template-syntax)
				 template-syntax
				 (current-context)))))
       (else
	(raise-syntax-error template-syntax "invalid subtemplate")
	(values #f #f)))))

  (define (compile-list-template template-syntax variable-map depth)

    (define template (unwrap-syntax template-syntax))

    (define reversed-slots '())
    (define slot-table (imap (make-comparator integer? = < #f)))
    (define index 0)
    
    (define (template-element-compile! element)
      (receive (slots transcriber)
	  (compile-subtemplate (template-element-syntax element)
			       variable-map
			       (if (template-element-repeated? element)
				   (+ 1 depth)
				   depth))
	(and
	 transcriber
	 (cond
	  ((and (template-element-repeated? element)
		(= (vector-length slots) 0))
	   (raise-syntax-error (template-element-syntax element)
			       "no pattern variable to repeat here"))
	  (else
	   (vector-for-each
	    (lambda (slot)
	      (call-with-current-continuation
	       (lambda (abort)
		 (receive (updated-table ret)
		     (imap-search slot-table
				  slot
				  (lambda (insert ignore)
				    (insert index #f))
				  abort)
		   (set! slot-table updated-table)
		   (set! reversed-slots (cons slot reversed-slots))
		   (set! index (+ 1 index))))))
	    slots)
	   (if (template-element-repeated? element)
	       (lambda (match pattern-variables output)
		 (let loop ((match*-vector
			     (vector-map
			      (lambda (slot)
				(vector-ref match
					    (imap-ref slot-table slot)))
			      slots)))
		   (unless (vector-every null? match*-vector)
		     (cond
		      ((vector-any null? match*-vector)
		       (raise-syntax-error template-syntax
					   "output cannot be built"))
		      (else
		       (list-queue-add-back!
			output
			(transcriber (vector-map car match*-vector)
				     pattern-variables))
		       (loop (vector-map cdr match*-vector)))))))
	       (lambda (match pattern-variables output)
		 (list-queue-add-back!
		  output
		  (transcriber (vector-map
				(lambda (slot)
				  (vector-ref match (imap-ref slot-table slot)))
				slots)
			       pattern-variables)))))))))
	         
    (define-values (template-elements template-element-rest*)
      (analyze-template-list template))

    (define subtranscribers
      (map-in-order template-element-compile! template-elements))
    (define subtranscriber-rest*
      (map-in-order template-element-compile! template-element-rest*))

    (define (transcriber match pattern-variables)
      (let ((output (list-queue))
	    (output-rest (list-queue)))
	(for-each
	 (lambda (subtranscriber)
	   (subtranscriber match pattern-variables output))
	 subtranscribers)
	(unless (null? subtranscriber-rest*)
	  ((car subtranscriber-rest*) match pattern-variables output-rest))
	(let ((tail-syntax
	       (if (null? subtranscriber-rest*)
		   '()
		   (let ((tail
			  (unwrap-syntax (list-queue-front output-rest))))
		     (if (or (pair? tail) (null? tail))
			 tail
			 (list-queue-front output-rest))))))
	  (receive (first last)
	      (list-queue-first-last output)
	    (if (null? last)
		(set! first tail-syntax)
		(set-cdr! last tail-syntax))
	    (derive-syntax first template-syntax (current-context))))))
      
    (values (list->vector (reverse reversed-slots))
	    transcriber))

  (define (analyze-template-list list)
    (let loop ((list list) (reversed-elements '()) (index 0))
      (cond
       ((null? list)
	(values (reverse reversed-elements) '()))
       ((pair? list)
	(let ((template-syntax (car list)))
	  (if (and (pair? (cdr list)) (ellipsis? (unwrap-syntax (cadr list))))
	      (loop (cddr list)
		    (cons (make-template-element template-syntax #t index)
			  reversed-elements)
		    (+ 2 index))
	      (loop (cdr list)
		    (cons (make-template-element template-syntax #f index)
			  reversed-elements)
		    (+ 1 index)))))
       (else
	(values (reverse reversed-elements)
		`(,(make-template-element list #f index)))))))
  
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
     
     (parameterize ((current-rename rename)
		    (current-compare compare)
		    (current-context syntax))
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
	   (loop (cdr rules)))))))))

(define (make-rule matcher transcriber)
  (vector matcher transcriber))
(define (rule-match rule syntax)
  ((vector-ref rule 0) syntax))
(define (rule-transcribe rule match)
  ((vector-ref rule 1) match))

;;; Concrete data types used in the pattern compiler

(define (make-pattern-variable index depth syntax) (vector index depth syntax))
(define (pattern-variable-index variable) (vector-ref variable 0))
(define (pattern-variable-depth variable) (vector-ref variable 1))
(define (pattern-variable-syntax variable) (vector-ref variable 2))

(define (make-pattern-variable-map) (imap identifier-comparator))

;; A pattern element is a vector consisting of four entries. The first
;; entry is the syntax of the element, the second one the index in the
;; list to match, the third one is a boolean specifying whether to
;; count the index from the front or the back and the last entry is a
;; boolean specifying whether the entry itself is repeated.

(define (make-pattern-element syntax index from-end? repeated?)
  (vector syntax index from-end? repeated?))
(define (pattern-element-syntax element) (vector-ref element 0))
(define (pattern-element-index element) (vector-ref element 1))
(define (pattern-element-from-end? element) (vector-ref element 2))
(define (pattern-element-repeated? element) (vector-ref element 3))
(define (pattern-element-set-repeated?! element value)
  (vector-set! element 3 value))

;;; Concrete data types used in the template compiler

(define (make-template-element syntax repeated? index)
  (vector syntax repeated? index))
(define (template-element-syntax template-element)
  (vector-ref template-element 0))
(define (template-element-repeated? template-element)
  (vector-ref template-element 1))
(define (template-element-index template-element)
  (vector-ref template-element 2))

;;; Utility functions

(define (constant? datum)
  (or (char? datum)
      (string? datum)
      (boolean? datum)
      (number? datum)
      (bytevector? datum)
      (vector? datum)))

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
