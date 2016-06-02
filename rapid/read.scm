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

(define-record-type <source-port>
  (%make-source-port port source ci? line column)
  source-port?
  (port source-port-port)
  (source source-port-source)
  (ci? source-port-ci? source-port-set-ci?!)
  (line source-port-line source-port-set-line!)
  (column source-port-column source-port-set-column!))

(define (make-source-port port source ci?)
  (%make-source-port-port port source ci? 1 0))

(define (source-port-read-char source-port)
  (let ((char (read-char (source-port-port source-port))))
    (cond
     ((eof-object? char) char)
     (else
      (case char
	((#\tab)
	 (source-port-set-column! source-port
				  (* (+ (quotient (source-port-column source-port) 8)
					1)
				     8)))
	((#\newline)
	 (source-port-set-column! source-port 0)
	 (source-port-set-line! source-port (+ (source-port-line source-port) 1)))
	((#\return)
	 (source-port-set-column! source-port 0))
	(else
	 ;; FIXME: Handle Unicode character width correctly.
	 ;; See also <https://www.gnu.org/prep/standards/standards.html#Errors>.
	 (source-port-set-column! source-port
				  (+ (source-port-column source-port) 1))))))))

(define (source-port-fold-case! source-port)
  (source-port-set-ci?! source-port #t))

(define (source-port-no-fold-case! source-port)
  (source-port-set-ci?! source-port #f))

(define (source-port-position source-port)
  (make-position (source-port-line source-port)
		 (source-port-column source-port)))

(define (source-port-make-location source-port start end)
  (make-source-location (source-port-source source-port) start end))

;;; Reader errors


(define (read-syntax source-port context)
  )
