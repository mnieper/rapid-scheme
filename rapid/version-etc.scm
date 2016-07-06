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

(define (version-etc command-name)
  (write-string (format "~a (Rapid Scheme) 0.1.1~%" command-name))
  (write-string
   "Copyright © 2016 Marc Nieper-Wißkirchen\n\
    License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n\
    This is free software: you are free to change and redistribute it.\n\
    There is NO WARRANTY, to the extent permitted by law.\n"))

(define (emit-bug-reporting-address)
  (write-string
   "Email bug reports to: marc@nieper-wisskirchen.de.\n"))
