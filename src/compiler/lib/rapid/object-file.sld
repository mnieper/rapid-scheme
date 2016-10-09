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

(define-library (rapid object-file)
  (export make-object-file
	  object-file?
	  object-file-make-section
	  object-file-get-text-section
	  object-file-get-data-section
	  object-file-get-bss-section
	  output-object-file
	  object-file-section-set-size!
	  object-file-section-set-contents!
	  object-file-section-set-alignment!
	  object-file-section-add-global!
	  object-file-section-add-reloc!)
  (import (scheme base)
	  (scheme file))
  (include "object-file.scm"))
