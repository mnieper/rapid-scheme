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

(define (closure-convert expr store)
  (let* ((free-vars-env (make-free-variables-environment expr))
	 (expr (partition-bindings expr free-vars-env)))
    (mark-escaping-procedures! expr store)
    (let convert ((expr expr) (env (imap eq?)))
      (match expr
	((receive (,var* ...) (,operator ,(operand*) ...)
	   ,expr)
	 `(receive ,var* (,operator ,@operand*)
	    ,(let loop ((env env) (vars var*))
	       (if (null? vars)
		   (convert expr env)
		   (loop (imap-replace env (car vars) (car vars)) (cdr vars))))))
	((if ,(test) ,(consequent) ,(alternate))
	 `(if ,test ,consequent ,alternate))

	;; letrec <-- komplex

	;; (operand operator ...) <-- ebenfalls komplex
		
	(,x x)))
    
    
    ;; Algorithm:
    ;;   - Create a map that maps variables to expression or BOTTOM
    ;;     that map is threaded through the ast walking code
    ;;     Aliases for other variables or constants are mapped to the actual value
    ;;     If x is unbound it is mapped to BOTTOM
    ;;   - At the same time determine required free variables
    ))
