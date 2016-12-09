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

(define (partition-bindings expr env)
  (match expr
    ((receive (,var* ...) (,operator ,(operand*) ...))
     `(receive ,var* (,operator ,@operand*)))
    ((if ,(test) ,(consequent) ,(alternate))
     `(if ,test ,consequent ,alternate))
    ((letrec ((,name* (lambda (,formal** ...) ,(body*)))
	      ...)
       ,(expr))
     (let ((definitions (alist->imap eq? (map list name* formal** body*)))
	   (free-vars* (map (lambda (name) (free-variables name env)) name*)))
       (let ((graph (map (lambda (name)
			   (cons name
				 (let loop ((names name*) (free-vars* free-vars*))
				   (cond
				    ((null? names)
				     '())
				    ((or (eq? (car names) name)
					 (not (iset-member? (car free-vars*) name)))
				     (loop (cdr names) (cdr free-vars*)))
				    (else
				     (cons (car names)
					   (loop (cdr names) (cdr free-vars*))))))))
			 name*)))
	 (receive (scc) (graph-scc graph eq?)
	   (let loop ((scc scc))
	     (if (null? scc)
		 expr
		 `(letrec ,(map (lambda (name)
				  (receive (formals body)
				      (apply values (imap-ref definitions name))
				    `(,name (lambda ,formals ,body))))
				(car scc))
		    ,(loop (cdr scc)))))))))
    (((,operator) ,(operand*) ...)
     `(,operator ,@operand*))
    (,x x)))
