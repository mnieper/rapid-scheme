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

(define-record-type <import-set>
  (%make-import-set library-name-syntax modifier)
  import-set?
  (library-name-syntax import-set-library-name-syntax)
  (modifier import-set-modifier))

(define (make-import-set syntax)
  (let-values
      (((library-name-syntax modifier)
	(let loop ((syntax syntax))
	  (let ((datum (syntax-datum syntax)))
	    (cond
	     ((list? datum)
	      (if (and (> (length datum) 1) (list? (syntax-datum (cadr datum))))
		  (let-values (((library-name-syntax modifier)
				(loop (cadr syntax))))
		    (case (syntax-datum (car datum))
		      ((only)
		       (and (every identifier? (cddr datum))
			    (values library-name-syntax
				    (only-modifier modifier (cddr datum)))))
		      ((except)
		       (and (every identifier? (cddr datum))
			    (values library-name-syntax
				    (except-modifier modifier (cddr datum)))))
		      ((prefix)
		       (cond
			((= (length datum) 3)
			 (and (identifier? (list-ref datum 2))
			      (values library-name-syntax
				      (except-modifier modifier
						       (list-ref datum 2)))))
			(else
			 (raise-syntax-error syntax "bad prefix import set")
			 #f)))
		      ((rename)
		       (and (every rename? (cddr datum))
			    (values library-name-syntax
				    (rename-modifier modifier (cddr datum)))))
		      (else
		       (raise-syntax-error syntax "invalid import set"))))
		    (and (library-name? syntax)
			 (values syntax (lambda (exports) exports)))))
	     (else
	      (raise-syntax-error syntax "bad import set")
	      #f))))))
    (%make-import-set library-name-syntax modifier)))

(define (import-set-modify import-set exports)
  (import-set-modifier import-set) exports)

(define (only-modifier modifier syntax*)
  (lambda (exports)
    (let ((exports (modifier exports)))      
      (let loop ((only-exports
		  (imap identifier-comparator))
		 (syntax*
		  syntax*))
	(if (null? syntax*)
	    only-exports
	    (let ((datum (syntax-datum (car syntax*))))
	      (cond
	       ((imap-ref/default exports datum #f)
		=> (lambda (identifier-syntax)
		     (loop (imap-replace only-exports datum identifier-syntax)
			   (cdr syntax*))))
	       (else
		(raise-syntax-error (car syntax*)
				    "identifier in only import set not found")
		#f))))))))

(define (except-modifier modifier syntax*)
  (lambda (exports)
    (let loop ((exports (modifier exports))
	       (syntax* syntax*))
      (if (null? syntax*)
	  exports
	  (let*-values
	      (((datum) (syntax-datum (car syntax*)))
	       ((exports ok)	    
		(imap-search
		 exports
		 datum
		 (lambda (insert ignore)
		   (raise-syntax-error
		    (car syntax*)
		    "identifier in except modifier set not found")
		   (ignore #f))
		 (lambda (key update remove)
		   (remove #t)))))
	    (and ok (loop exports (cdr syntax*))))))))

(define (prefix-modifier modifier syntax*)
  (lambda (exports)
    (modifier exports)))

(define (rename-modifier modifier syntax*)
  (lambda (exports)
    (modifier exports)))

(define library-name?
  (case-lambda
   ((syntax)
    (library-name? syntax raise-syntax-error))
   ((syntax raise)
    (let ((datum (syntax-datum syntax)))
      (or (and (list? datum)
	       (let loop ((datum datum))
		 (or (null? datum)
		     (let ((element (syntax-datum (car datum))))
		       (and (or (and (exact-integer? element) (>= element 0))
				(symbol? element))
			    (loop (cdr datum)))))))
	  (begin (raise syntax "bad library name")
		 #f))))))

(define (identifier? syntax)
  (or (symbol? (syntax-datum syntax))
      (begin
	(raise-syntax-error syntax "bad identifier")
	#f)))

(define (rename? syntax)
  (let ((datum (syntax-datum syntax)))
    (cond
     ((and (list? datum) (= (length datum) 2))
      (and (identifier? (car datum))
	   (identifier? (cadr datum))))
     (else
      (raise-syntax-error syntax "bad rename")
      #f))))

(define identifier-comparator
  (make-comparator symbol? symbol=?
		   (lambda (x y)
		     (string<? (symbol->string x) (symbol->string y)))
		   #f))
