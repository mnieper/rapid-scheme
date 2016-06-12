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

(define-syntax define-auxiliary-syntax
  (er-macro-transformer
   (lambda (syntax rename compare)
     (define _er-macro-transformer (rename 'er-macro-transformer))
     (define _define-syntax (rename 'define-syntax))
     `(,_define-syntax
       ,(cadr (syntax-datum syntax))
       (,_er-macro-transformer
	(lambda (syntax rename compare)
	  (compile-error (format "invalid use of auxiliary syntax ‘~a’"
				 (syntax->datum (car (syntax-datum syntax)) unclose-form))
			 syntax)))))))

(define-syntax ... (make-auxiliary-syntax))
(define-syntax _ (make-auxiliary-syntax))

(define-syntax %syntax-rules ;; XXX
  (er-macro-transformer
   (lambda (syntax rename compare)
     (and-let*
	 ((transformer (syntax-datum syntax))
	  (ellipsis-syntax (cond
			    ((and (>= (length transformer) 2)
				  (list (syntax-datum (list-ref transformer 1))))
			     #f)
			    ((and (>= (length transformer) 3)
				  (identifier? (syntax-datum (list-ref transformer 1)))
				  (list? (syntax-datum (list-ref transformer 2))))
			     (list-ref transformer 1))
			    (else
			     (compile-error "bad syntax-rules syntax" transformer-syntax))))
	  (literal-syntax* (list-ref transformer (if ellipsis-syntax 2 1)))
	  (syntax-rule-syntax* (list-tail transformer (if ellipsis-syntax 3 2)))
	  (ellipsis (if ellipsis-syntax (syntax-datum ellipsis-syntax) #f))
	  (identifier-comparator (make-comparator identifier? compare #f #f))
	  (literal-set
	   (let loop ((literal-set (make-set identifier-comparator))
		      (literal-syntax* literal-syntax+))
	     (if (null? literal-syntax*)
		 literal-set
		 (let*
		     ((literal-syntax (car literal-syntax*))
		      (literal (syntax-datum literal-syntax)))
		   (assert-identifier! literal-syntax)
		   (when (set-contains? literal-set literal)
		     (compile-error (format "duplicate literal identifier ‘~a’"
					    (syntax->datum literal-syntax unclose-form))
				    literal-syntax))
		   (loop (set-adjoin literal-set literal) (cdr literal-syntax*))))))
	  (literal? (lambda (identifier)
		      (set-contains? literal-set identifier)))
	  (ellipsis? (if ellipsis
			 (lambda (form)
			   (and (identifier? form)
				(not (literal? form))			   
				(compare ellipsis form #t)))  ;; this won't work!
			 (lambda (form)
			   (and (identifier? form)
				(not (literal? form))
				(compare (rename '...) form)))))
	  (underscore? (lambda (identifier)
			 (compare (rename '_) identifier)))
	  

	  
	  
     )))
))
 
