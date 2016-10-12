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
	(rapid module)
	(rapid codegen))

(define (main)
  (define codegen (make-codegen))
  (define module (make-module codegen))
  (define var (module-add-var module 42))
  (define datum (module-add-datum module #u8(#xDE #xAD #xBE #xEF)))
  (codegen-add-module! module)
  (codegen-set-var! var (module-datum-reference datum))
  (codegen-emit codegen "bootstrap.s" (module-datum-reference datum)))

(main)
 
