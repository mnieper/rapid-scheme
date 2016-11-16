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
  '((goto (label start1))
    (compare (reg 0) (reg 1))
    (branch (cond =) (label start2))
    (halt)
    start1
    (assign 0 (local start2))
    (goto (reg 0))
    start2
    ;; TODO: Better mnemonics!
    (assign 3 (local v))
    (assign 4 (local start3))
    (set! 3 (const 0) (reg 4))
    (goto (ref 3 (const 0)))
    start3
    (assign 0 (local msg))
    (assign 1 (global stdout))
    (call fputs)
    (assign 0 (local var))
    (assign 0 (op +) (ref 0 (const 0)) (const 2))
    (call exit)))

(define (main)
  (codegen-emit
   "bootstrap.s"
   `(program
     (modules
      (main
       (module
        (procedures
   	 (proc ,code))
	(data
	 (msg ,(string->utf8 "Hello, World!\n")))
	(variables
	 (v 0)
	 (var 42)))))
     (inits #;((main var) (main msg)))
     (entry (main proc)))))

(main)
