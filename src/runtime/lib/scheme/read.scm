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

(define read
  (case-lambda
   (() (read (current-input-port)))
   ((port)
    (let*
	((source-port
	  (make-source-port port #f (port-ci? port)))
	 (syntax
	  (guard (condition (else (raise (make-read-error condition)))) 
	    (read-syntax source-port #f))))
      (port-set-ci?! port (source-port-ci? source-port))
      (if syntax
	  (syntax->datum syntax)
	  (eof-object))))))
     
