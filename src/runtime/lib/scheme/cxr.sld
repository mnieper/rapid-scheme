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

(define-library (scheme cxr)
  (export caaar
	  caadr
	  cadar
	  caddr
	  cdaar
	  cdadr
	  cddar
	  cdddr
	  caaaar
	  caaadr
	  caadar
	  caaddr
	  cadaar
	  cadadr
	  caddar
	  cadddr
	  cdaaar
	  cdaadr
	  cdadar
	  cdaddr
	  cddaar
	  cddadr
	  cdddar
	  cddddr)
  (import (only (rapid primitives)
		caadr
		cadar
		caddr
		cdaar
		cdadr
		cddar
		cdddr
		caaaar
		caaadr
		caadar
		caaddr
		cadaar
		cadadr
		caddar
		cadddr
		cdaaar
		cdaadr
		cdadar
		cdaddr
		cddaar
		cddadr
		cdddar
		cddddr)))
