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


;;;; List all instructions that we need
;;;;   -> alloc a b (just gc with pointer to proc and mask!)
;;;;   -> (perform (op puts) (reg 1) (reg 2) ...)
;;;;   -> anstelle von (reg i) können wir auch (off v 3) oder (ref 29) schreiben
;;;;   ->    bytevectors/andere prozeduren werden mit rip-relativem Label addressiert.

(define-record-type <lir-label>
  (make-lir-label)
  lir-label?)

(define (lir-assemble code)
  (for-each
   (lambda (inst)
     (assemble-instruction inst))
   code))
  
