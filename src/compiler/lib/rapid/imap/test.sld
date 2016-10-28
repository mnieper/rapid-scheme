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

(define-library (rapid imap test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid imap))
  (begin
    (define (run-tests)
      (test-begin "imap")
      
      (test-assert "make-imap"
	(imap? (make-imap eq?)))

      (test-equal "imap-ref"
	'(foo bar foo)
	(let* ((map (make-imap eq?))
	       (map (imap-replace map 1 'foo))
	       (map (imap-replace map 2 'bar)))
	  (list (imap-ref map 1) (imap-ref map 2) (imap-ref map 1))))

      (test-equal "imap-ref"
	'(foo bar foo)
	(let* ((map (make-imap eq?))
	       (map (imap-replace map 1 'foo))
	       (map (imap-replace map 2 'bar)))
	  (list (imap-ref map 1) (imap-ref map 2) (imap-ref map 1))))

      (test-equal "imap-ref/default"
	'(qux foo bar foo)
	(let* ((map (make-imap eq?))
	       (map (imap-replace map 1 'foo))
	       (map (imap-replace map 2 'bar)))
	  (list (imap-ref/default map 3 'qux)
		(imap-ref map 1) (imap-ref map 2) (imap-ref map 1))))
      
      (test-equal "imap-replace"
	'(qux bar frob)
	(let* ((map (make-imap eq?))
	       (map (imap-replace map 1 'foo))
	       (map (imap-replace map 2 'bar))
	       (map (imap-replace map 3 'frob))
	       (map (imap-replace map 1 'qux)))
	  (list (imap-ref map 1) (imap-ref map 2) (imap-ref map 3))))
      
      (test-end))))
