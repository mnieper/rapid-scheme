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

(import (rapid primitive))

(define-primitive display 'display)
(define-primitive newline 'newline)
(define-primitive apply 'apply)
(define-primitive list 'list)
(define-primitive call/cc 'call/cc)

(define-values (values)
  (case-lambda
   (a  
    (call/cc
     (case-lambda
      ((cont)
       (apply cont a)))))))

(define-values (x y)
  (begin
    (values 1 2 (display "ok\n"))
    (values 'a 'b)))

(display (list x y))
(newline)

;; Local Variables:
;; mode: scheme
;; End:
