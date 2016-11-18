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
	(rapid compiler backend module)
	(rapid compiler backend codegen))

(define code
  '((jump start1)
    (record (var 1 2 3) r6)
    (branch r0 r1 (= start2))
    (halt)
    start1
    (move start2 r0)
    (jump r0)
    start2
    (lea (0 v) r3)
    (lea (0 start3) r4)
    (store (0 r3) r4)
    (load (0 r3) r3)
    (jump r3)
    start3
    (lea (0 msg) r0)
    (global-fetch stdout r1)
    (call fputs)
    (load (0 var) r0)
    (add r0 2 r0)
    (call exit)))

(define (main)
  (codegen-emit
   "bootstrap.s"
   `(program
     (entry (main proc))
     (module main
	     (procedure proc
			,@code)
	     (data msg ,(string->utf8 "Hello, World!\n"))
	     (variable v 0)
	     (variable var 42)))))

(main)
