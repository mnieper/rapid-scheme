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
	(rapid object-file))

(define (main)
  (define object-file (make-object-file))
  (define rapid-text-section (object-file-make-section object-file "rapid_text" '(alloc write execinstr) #t))
  (object-file-section-add-global! rapid-text-section "rapid_run" 0)

  (when (file-exists? "bootstrap.s")
    (delete-file "bootstrap.s"))
  (output-object-file object-file "bootstrap.s"))

(main)
