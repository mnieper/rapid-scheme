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

(define-library (rapid compiler procedure-store)
  (export make-procedure-store
	  mark-escaping-procedure!
	  mark-continuation!
	  escaping-procedure?
	  continuation?
	  store-procedure-definition!
	  procedure-definition
	  set-argument-registers!
	  get-argument-registers)
  (import (scheme base)
	  (rapid match)
	  (rapid imap))
  (include "procedure-store.scm"))