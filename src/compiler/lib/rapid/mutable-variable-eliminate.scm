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

;; XXX: Can procedure labels be mutable?

(define (mutable-variable-eliminate exp)

  ;; Mark mutable locations
  (parameterize
      ((current-procedure-method
	(lambda (exp)
	  (for-each
	   (lambda (clause)
	     (let ((formals (clause-formals clause)))
	       (for-each init-location! (formals-fixed formals))
	       (for-each init-location! (formals-rest formals)))
	     (for-each mark-locations! (clause-body clause)))
	   (procedure-clauses exp))
	  exp))
       (current-letrec-expression-method
	(lambda (exp)
	  (for-each
	   (lambda (variables)
	     (init-location! (car (formals-fixed (variables-formals variables))))
	     (mark-locations! (variables-expression variables)))
	   (letrec-expression-definitions exp))
	  (for-each mark-locations! (letrec-expression-body exp))
	  exp))
       (current-assignment-method
	(lambda (exp)
	  (mark-location! (assignment-location exp))
	  (mark-locations! (assignment-expression exp))
	  exp)))
    (mark-locations! exp))

  ;; Convert mutable locations into cells
  (parameterize
      ((current-reference-method
	(lambda (exp)
	  (let ((location (reference-location exp)))
	    (if (and (location? location) (mutable? location))
		(let ((primitive/cell-ref (make-primitive 'cell-ref #f)))
		  (expression (primitive/cell-ref ,exp)))
		exp))))
       (current-assignment-method
	(lambda (exp)
	  (let*
	      ((location (assignment-location exp))
	       (primitive/cell-set! (make-primitive 'cell-set! #f)))
	    (expression (primitive/cell-set! location
					     ,(eliminate
					       (assignment-expression exp)))))))
       (current-letrec-expression-method
	(lambda (exp)
	  (let*-values
	      (((definitions) (letrec-expression-definitions exp))
	       ((labels new-locations mutable-locations)
		(let loop ((definitions definitions))
		  (if (null? definitions)
		      (values '() '() '())
		      (let*-values
			  (((labels new-locations mutable-locations)
			    (loop (cdr definitions)))
			   ((label)
			    (car (formals-fixed (variables-formals
						 (car definitions))))))
			(if (mutable? label)
			    (let ((new-location (make-location
						 (location-syntax label))))
			      (values (cons new-location labels)
				      (cons new-location new-locations)
				      (cons label mutable-locations)))
			    (values (cons label labels)
				    new-locations
				    mutable-locations)))))))
	    (make-letrec-expression
	     (map (lambda (label definition)
		    (make-variables (make-formals (list label) '()
						  (formals-syntax
						   (variables-formals definition)))
				    (eliminate (variables-expression definition))
				    (variables-syntax definition)))
		  labels definitions)
	     (with-syntax exp
	       (eliminate-body new-locations
			       mutable-locations
			       (letrec-expression-body exp)))
	     #f))))
       (current-procedure-method
	(lambda (exp)
	  (expression
	   (case-lambda
	    ,@(map
	       (lambda (clause)
		 (let*-values
		     (((formals)
		       (clause-formals clause))
		      ((new-fixed new-locations mutable-locations)
		       (let loop ((fixed (formals-fixed formals)))
			 (if (null? fixed)
			     (values '() '() '())
			     (let-values
				 (((new-fixed new-locations mutable-locations)
				   (loop (cdr fixed))))
			       (if (mutable? (car fixed))
				   (let ((location (make-location
						    (location-syntax (car fixed)))))
				     (values (cons location new-fixed)
					     (cons location new-locations)
					     (cons (car fixed) mutable-locations)))
				   (values (cons (car fixed) new-fixed)
					   new-locations
					   mutable-locations))))))
		      ((new-rest* new-locations mutable-locations)
		       (let ((rest* (formals-rest formals)))
			 (if (null? rest*)
			     (values '() new-locations mutable-locations)
			     (if (mutable? (car rest*))
				 (let ((location (make-location
						  (location-syntax (car rest*)))))
				   (values (list location)
					   (cons location new-locations)
					   (cons (car rest*) mutable-locations)))
				 (values rest* new-locations mutable-locations))))))
		   (with-syntax (clause-syntax clause)
		     (make-clause
		      (make-formals new-fixed new-rest* (formals-syntax formals))
		      (eliminate-body new-locations mutable-locations
				      (clause-body clause))
		      #f))))
	       (procedure-clauses exp)))))))
    (eliminate exp)))

(define (eliminate-body new-locations mutable-locations body)
  (let ((body (map eliminate body)))
    (if (null? new-locations)
	body
	(list
	 (expression
	  ((case-lambda
	    ,@(list
	       (make-clause
		(make-formals
		 mutable-locations
		 '()
		 #f)
		body
		#f)))
	   ,@(map (lambda (new-location)
		    (let ((primitive/make-cell
			   (make-primitive 'make-cell #f)))
		      (expression
		       (primitive/make-cell new-location))))
		  new-locations)))))))

(define (mark-locations! exp)
  (expression-dispatch exp))

(define (eliminate exp)
  (expression-dispatch exp))

(define (init-location! location)
  (denotation-set-aux! location #f))

(define (mark-location! location)
  (denotation-set-aux! location #t))

(define (mutable? location)
  (denotation-aux location))
