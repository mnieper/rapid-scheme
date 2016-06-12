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

(define-primitive %display 'display)
(define-primitive %write 'write)
(define-primitive %write-shared 'write-shared)
(define-primitive %write-simple 'write-simple)

(define display
  (case-lambda
   ((obj) (%display obj (current-output-port)))
   ((obj port) (%display obj port))))

(define write
  (case-lambda
   ((obj) (%write obj (current-output-port)))
   ((obj port) (%write obj port))))

(define write-shared
  (case-lambda
   ((obj) (%write-shared obj (current-output-port)))
   ((obj port) (%write-shared obj port))))

(define write-simple
  (case-lambda
   ((obj) (%write-simple obj (current-output-port)))
   ((obj port) (%write-simple obj port))))
