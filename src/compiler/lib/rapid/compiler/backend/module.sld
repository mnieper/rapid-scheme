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

(define-library (rapid compiler backend module)
  (export make-module
	  module?
	  module-code
	  module-reference?
	  module-reference
	  module-reference-module
	  module-reference-offset)
  (import (scheme base)
	  (scheme cxr)
	  (rapid imap)
	  (rapid match)
	  (rapid compiler identifier)
	  (rapid compiler backend assembler)
	  (rapid compiler backend runtime))
  (include "module.scm"))
