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

(define (make-er-macro-transformer transformer)
  (let ((macro-environment (current-syntactic-environment)))
    (lambda (syntax)
      (let ((environment (current-syntactic-environment)))      
      
	(define renames (imap identifier-comparator))
	
	(define (rename identifier)
	  (cond
	   ((imap-ref renames identifier #f))
	   (else
	    (let ((renamed-identifier (close-syntax identifier macro-environment)))
	      (set! renames (imap-replace identifier renamed-identifier))
	      renamed-identifier))))

	(define (compare identifier1 identifier2)
	  (identifier=? environment identifier1 environment identifier2))

	(transformer syntax rename compare)))))

(define (make-syntax-rules-transformer
	 ellipsis? literal? underscore? syntax-rule-syntax* transformer-syntax)
  (error "make-syntax-rules-transformer: not implemented yet"))
