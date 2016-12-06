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

(import (scheme base)
	(scheme cxr)
	(scheme read)
	(scheme process-context)
	(rapid compiler environment)
	(rapid compiler assign-registers)
	(rapid compiler generate-module)
	(rapid compiler backend codegen))

(define (generate-program code)
  (let ((environment (make-environment)))
    (assign-registers! (cdr code) environment)    
    `(program
      (entry (main main))
      (module main
 	      ,@(cdr (generate-module (cdr code) environment))))))

(define (main)
  (let* ((code (read))
	 (code (if (eq? 'define (caadr code))
		   (generate-program code)
		   code)))	
    (let ((output-file-name (list-ref (command-line) 1)))
      (codegen-emit output-file-name code))))

(main)
