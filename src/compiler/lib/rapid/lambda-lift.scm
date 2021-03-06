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

(define (lambda-lift root-expression)
  
  (define (set-binding-construct! location binding-construct)
    (denotation-set-aux! location binding-construct))

  (define (lookup-binding-construct location)
    (denotation-aux location))

  (define (set-depth! binding-construct depth)
    (expression-set-aux! binding-construct (vector depth '())))

  (define (insert-binding! binding-construct variables)
    (let ((vector (expression-aux binding-construct)))
      (vector-set! vector 1 (cons variables (vector-ref vector 1)))))
  
  (define (binding-construct-depth binding-construct)
    (vector-ref (expression-aux binding-construct) 0))

  (define (binding-construct-bindings binding-construct)
    (vector-ref (expression-aux binding-construct) 1))
  
  (define binding-construct-deepest
    (case-lambda
     (()
      root-expression)
     ((binding-construct)
      binding-construct)
     ((binding-construct1 binding-construct2)
      (if (>= (binding-construct-depth binding-construct1)
	      (binding-construct-depth binding-construct2))
	  binding-construct1
	  binding-construct2))
     ((binding-construct . binding-construct*)
      (binding-construct-deepest binding-construct
				 (apply binding-construct-deepest binding-construct*)))))

  (define (binding-construct-highest binding-construct1 binding-construct2)
    (if (<= (binding-construct-depth binding-construct1)
	    (binding-construct-depth binding-construct2))
	binding-construct1
	binding-construct2))
  
  (define (lift expression depth)
    (expression-dispatch expression depth))

  (define lift-procedure
    (case-lambda
     ((expression depth)
      (lift-procedure expression depth #f))
     ((expression depth not-simple?)
      (let ((syntax (expression-syntax expression))
	    (clauses (procedure-clauses expression)))
	(receive (binding-construct clause*)
	    (do-lift* lift-procedure-clause clauses depth)
	  (let ((procedure (make-procedure
			    clause*
			    syntax)))
	    (if (and (>= (binding-construct-depth binding-construct)
			 (- depth 1))
		     (not not-simple?))
		;; The procedure cannot be lifted further.
		(values binding-construct procedure)
		;; The procedure is to be lifted.
		(let ((f (make-location syntax)))
		  (insert-binding! binding-construct
				   (make-variables (make-formals (list f) syntax)
						   procedure
						   syntax))
		  (values binding-construct
			  (make-reference f syntax))))))))))

  (define (lift-procedure-clause clause depth)
    (let*
	((formals (clause-formals clause))
	 (locations (formals-locations formals))
	 ;; The following expression is a letrec*-expression construct.
	 (expression (car (clause-body clause))))
      (for-each
       (lambda (location)
	 (set-binding-construct! location expression))
       locations)
      (receive (binding-construct lifted-expression)
	  (lift expression (+ depth 1))
	(values binding-construct
		(make-clause formals
			     (list lifted-expression)
			     (clause-syntax clause))))))
  
  (define (lift-letrec*-expression expression depth)
    (let ((definitions (letrec*-expression-definitions expression)))
      (set-depth! expression depth)
      (for-each
       (lambda (variables)
	 (for-each
	  (lambda (location)
	    (set-binding-construct! location expression))
	  (formals-locations (variables-formals variables))))
       definitions)
      (receive (binding-construct1 init*)
	  (do-lift* lift-definition definitions (+ depth 1))
	(receive (binding-construct2 lifted-body)
	    (lift* (letrec*-expression-body expression) (+ depth 2))
	  (values (binding-construct-highest
		   expression
		   (binding-construct-deepest binding-construct1
					      binding-construct2))
		  (make-letrec*-expression
		   (append
		    (binding-construct-bindings expression)
		    (map
		     (lambda (variables init)
		       (make-variables (variables-formals variables)
				       init
				       (variables-syntax variables)))
		     definitions init*))
		   lifted-body
		   (expression-syntax expression)))))))

  (define (lift-definition definition depth)
    (let ((formals (variables-formals definition))
	  (expression (variables-expression definition)))
      (if (expression-procedure? expression)
	  (lift-procedure expression depth (not (formals-location formals)))
	  (lift expression depth))))
  
  (define (lift* expression* depth)
    (do-lift* lift expression* depth))

  (define (do-lift* lifter expression* depth)
    (let ((lift*  
	   (map
	    (lambda (expression)
	      (receive lift
		  (lifter expression depth)
		lift))
	    expression*)))
      (values (apply binding-construct-deepest (map car lift*))
	      (map cadr lift*))))

  (parameterize
      ((current-literal-method
	(lambda (expression depth)
	  (values root-expression
		  expression)))
       (current-undefined-method
	(lambda (expression depth)
	  (values root-expression
		  expression)))
       (current-reference-method
	(lambda (expression depth)
	  (let ((location (reference-location expression)))
	    (if (primitive? location)
		(values root-expression
			expression)
		(values (lookup-binding-construct (reference-location expression))
			expression)))))
       (current-procedure-call-method
	(lambda (expression depth)
	  (receive (binding-construct expression*)
	      (lift* (cons (procedure-call-operator expression)
			   (procedure-call-operands expression))
		     (+ depth 1))
	    (values binding-construct
		    (make-procedure-call (car expression*)
					 (cdr expression*)
					 (expression-syntax expression))))))
       (current-procedure-method
	lift-procedure)
       (current-assignment-method
	(lambda (expression depth)
	  (receive (binding-construct init)
	      (lift (assignment-expression expression) (+ depth 1))
	    (let ((location (assignment-location expression)))
	      (values (binding-construct-deepest
		       binding-construct
		       (lookup-binding-construct location))
		      (make-assignment location init
				       (expression-syntax expression)))))))
       (current-letrec*-expression-method
	lift-letrec*-expression)
       (current-sequence-method
	(lambda (expression depth)
	  (receive (binding-construct expression*)
	      (lift* (sequence-expressions expression) (+ depth 1))
	    (values binding-construct
		    (make-sequence expression*
				   (expression-syntax expression))))))
       (current-conditional-method
	(lambda (expression depth)
	  (receive (binding-construct expression*)
	      (lift* (list (conditional-test expression)
				(conditional-consequent expression)
				(conditional-alternate expression))
		     (+ depth 1))
	    (values binding-construct
		    (make-conditional (list-ref expression* 0)
				      (list-ref expression* 1)
				      (list-ref expression* 2)
				      (expression-syntax expression)))))))  
    (receive (_ lifted-expression)
	(lift root-expression 0)
      lifted-expression)))
