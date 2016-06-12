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

(define-primitive call-with-input-file 'call-with-input-file)
(define-primitive call-with-output-file 'call-with-output-file)
(define-primitive delete-file 'delete-file)
(define-primitive file-exists? 'file-exists?)
(define-primitive open-binary-input-file 'open-binary-input-file)
(define-primitive open-binary-output-file 'open-binary-output-file)
(define-primitive open-input-file 'open-input-file)
(define-primitive open-output-file 'open-output-file)

(define (with-input-from-file string thunk)
  (call-with-input-file
      (lambda (port)
	(parameterize
	    ((current-input-port port))
	  thunk))))

(define (with-output-to-file string thunk)
  (call-with-output-file
      (lambda (port)
	(parameterize
	    ((current-output-port port))
	  thunk))))
