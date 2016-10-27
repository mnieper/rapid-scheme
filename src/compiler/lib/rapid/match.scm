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

(define-syntax match
  (syntax-rules ()
    ((match `expr clause ...)
     (let ((e expr))
       (call-with-current-continuation
	(lambda (r)
	  (match-aux "or" `e r clause ... ())))))))

;; XXX: let* alone is not sufficient; and-let* neither
;; XXX: Where to automatically pack/unpack? -> should happen in match?
;; `could be the packer. What about the unpacker?

(define-syntax match-aux
  (syntax-rules (unquote =>)
    ((match-aux "or" `e r () ((ac* proc) ...))
     (or (let*
	     ac*
	   (call-with-current-continuation
	    (lambda (skip)	   
	      (call-with-values
		  (lambda ()
		    (proc (lambda () (skip #f))))
		r)))) ...
	 (if #f #f)))
    ((match-aux "or" `e r (c . c*) tc*)
     (match-aux "clause" c `e r c* tc*))
    ((match-aux "clause" (p => proc) `e r c* tc*)
     (match-aux "=>" (p proc) `e r c* tc*))
    ((match-aux "clause" (p . body) `e r c* tc*)
     (match-aux "=>" (p (lambda (skip) . body)) `e r c* tc*))
    ((match-aux "=>" (,x proc) `e r c* (tc ...))
     (match-aux "or" `e r c* (tc ... (((x e) proc)))))))
		 
     #;(let-syntax
	 ((qq (syntax-rules ..2 ()
		((qq template) `template))))
       (let-syntax
	   ((quasiquote
	     (syntax-rules ..3 (... unquote)
	       ((quasiquote (,a ... . b))
		(qq (,@a . b)))
	       ((quasiquote x)
		(qq x)))))
	 . body))
