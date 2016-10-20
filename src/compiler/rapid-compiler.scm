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
	(scheme write)
	(scheme file)
	(rapid compiler backend module)
	(rapid compiler backend codegen))

(define (main)
  (define module
    (make-module (list (list 'proc '(XXX)))
		 (list (list 'datum #u8(#xDE #xAD #xBE #xEF)))
		 (list (list 'var 42))))
  (codegen-emit "bootstrap.s"
		(list module)
		(list (list (module-reference module 'var)
			    (module-reference module 'datum)))
		(module-reference module 'proc)))

(main)
