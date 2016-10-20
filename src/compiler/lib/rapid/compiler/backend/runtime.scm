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

(define-record-type <global-symbol>
  (make-global-symbol name index init)
  global-symbol?
  (name global-symbol-name)
  (index %global-symbol-index)
  (init global-symbol-init))

(define global-symbols '())
(define global-symbol-count 0)
(define (define-global-symbol symbol init)
  (let ((global-symbol (make-global-symbol
			(string-append ".L"
				       (string-map (lambda (char)
						     (if (eq? char #\-)
							 #\_
							 char))
						   (symbol->string symbol)))
			global-symbol-count init)))
    (set! global-symbols (cons (cons symbol global-symbol)
			       global-symbols))
    (set! global-symbol-count (+ global-symbol-count 1))))
(define (global-symbol-index symbol)
  (cond
   ((assq symbol global-symbols) => (lambda (entry) (%global-symbol-index (cdr entry))))
   (else
    (error "unknown global symbol" symbol))))
(define (for-each-global-symbol proc)
  (for-each (lambda (entry)
	      (proc (cdr entry)))
	    (reverse global-symbols)))

(define (generate-global-symbols-file filename)
  (when (file-exists? filename)
    (delete-file filename))  
  (with-output-to-file filename
    (lambda ()
      (for-each-global-symbol
       (lambda (global-symbol)
	 (let ((name (global-symbol-name global-symbol))
	       (init (global-symbol-init global-symbol)))
	   (write-directive "set" name ". - .Lrapid_gst")
	   (write-directive "quad" init)))))))

