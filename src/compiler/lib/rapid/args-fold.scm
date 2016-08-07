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

;;; Copyright (c) 2002 Anthony Carrico
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-record-type <option>
  (option names required-arg? optional-arg? option-proc)
  option?
  (names option-names)
  (required-arg? option-required-arg?)
  (optional-arg? option-optional-arg?)
  (option-proc option-processor))

(define (args-fold args options unrecognized-option-proc operand-proc . seed*)
  (define (find-option name)
    (find (lambda (option)
	    (find (lambda (n)
		    (equal? name n))
		  (option-names option)))
	  options))
  (define (scan-operands operands seed*)
    (if (null? operands)
	(apply values seed*)
	(let-values ((seed* (apply operand-proc (car operands) seed*)))
	  (scan-operands (cdr operands) seed*))))  
  (let scan-args ((args args) (seed* seed*))
    (define (scan-short-options index shorts args seed*)
      (if (= index (string-length shorts))
	  (scan-args args seed*)
	  (let*
	      ((name
		(string-ref shorts index))
	       (option
		(or (find-option name)
		    (option (list name)
			    #f
			    #f
			    unrecognized-option-proc))))
	    (cond
	     ((and (< (+ index 1) (string-length shorts))
		   (or (option-required-arg? option)
		       (option-optional-arg? option)))
	      (let-values ((seed* (apply (option-processor option)
					 option
					 name
					 (string-copy shorts (+ index 1))
					 seed*)))
		(scan-args args seed*)))
	     ((and (option-required-arg? option)
		   (pair? args))
	      (let-values ((seed* (apply (option-processor option)
					 option
					 name
					 (car args)
					 seed*)))
		(scan-args (cdr args) seed*)))
	     (else
	      (let-values ((seed* (apply (option-processor option)
					 option
					 name
					 #f
					 seed*)))
		(scan-short-options (+ index 1) shorts args seed*)))))))
    (if (null? args)
	(apply values seed*)
	(let ((arg (car args))
	      (args (cdr args)))
	  (cond
	   ((string=? "--" arg)
	    (scan-operands args seed*))
	   ((and (> (string-length arg) 4)
		 (char=? #\- (string-ref arg 0))
		 (char=? #\- (string-ref arg 1))
		 (not (char=? #\= (string-ref arg 2)))
		 (let loop ((index 3))
		   (cond
		    ((= index (string-length arg))
		     #f)
		    ((char=? #\= (string-ref arg index))
		     index)
		    (else
		     (loop (+ index 1))))))
	    => (lambda (index)
		 (let*-values
		     (((name)
		       (string-copy arg 2 index))
		      ((option-arg)
		       (string-copy arg (+ index 1)))
		      ((option)
		       (or (find-option name)
			   (option (list name)
				   #t
				   #f
				   unrecognized-option-proc)))
		      (seed*
		       (apply (option-processor option)
			      option
			      name
			      option-arg
			      seed*)))
		   (scan-args args seed*))))
	   ((and (> (string-length arg) 3)
		 (char=? #\- (string-ref arg 0))
		 (char=? #\- (string-ref arg 1)))
	    (let*
		((name
		  (string-copy arg 2))
		 (option
		  (or (find-option name)
		      (option (list name)
			      #f
			      #f
			      unrecognized-option-proc))))
	      (if (and (option-required-arg? option)
		       (pair? args))
		  (let-values
		      ((seed* (apply (option-processor option)
				     option
				     name
				     (car args)
				     seed*)))
		    (scan-args (cdr args) seed*))
		  (let-values
		      ((seed* (apply (option-processor option)
				     option
				     name
				     #f
				     seed*)))
		    (scan-args args seed*)))))
	   ((and (> (string-length arg) 1)
		 (char=? #\- (string-ref arg 0)))
	    (let ((shorts (string-copy arg 1)))
	      (scan-short-options 0 shorts args seed*)))
	   (else
	    (let-values ((seed* (apply operand-proc arg seed*)))
	      (scan-args args seed*))))))))
