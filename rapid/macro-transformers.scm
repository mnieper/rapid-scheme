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

  (define pattern-syntax-vector
    (list->vector
     (map
      (lambda (syntax-rule-syntax)
	(let ((pattern-syntax (car (unwrap-syntax syntax-rule-syntax))))
	  (derive-syntax (cdr (unwrap-syntax pattern-syntax)) pattern-syntax)))
      syntax-rule-syntax*)))

  (define template-syntax-vector
    (list->vector
     (map
      (lambda (syntax-rule-syntax)
	(cadr (unwrap-syntax syntax-rule-syntax)))
      syntax-rule-syntax*)))

  (define matchers '())  ;; FIXME
  (define transcribers '()) ;; FIXME
  
  (make-er-macro-transformer
   (lambda (syntax rename compare)
     (let loop ((matchers matchers) (transcribers transcribers))
       (cond
	((null? matchers)
	 (raise-syntax-error syntax "no expansion for macro use")
	 (raise-syntax-note transformer-syntax
			    "the macro definition was here")
	 #f)
	(((car matchers) syntax)
	 => (car transcribers))
	(else
	 (loop (cdr matchers) (cdr transcribers))))))))
