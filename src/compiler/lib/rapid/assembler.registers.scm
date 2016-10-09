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

(define-register rax reg64 0 #f)
(define-register rbx reg64 3 #f)
(define-register rcx reg64 1 #f)
(define-register rdx reg64 2 #f)
(define-register rsi reg64 6 #f)
(define-register rdi reg64 7 #f)
(define-register rbp reg64 5 #f)
(define-register rsp reg64 4 #f)
(define-register r8 reg64 8 #f)
(define-register r9 reg64 9 #f)
(define-register r10 reg64 10 #f)
(define-register r11 reg64 11 #f)
(define-register r12 reg64 12 #f)
(define-register r13 reg64 13 #f)
(define-register r14 reg64 14 #f)
(define-register r15 reg64 15 #f)

(define-register eax reg32 0 #f)
(define-register ebx reg32 3 #f)
(define-register ecx reg32 1 #f)
(define-register edx reg32 2 #f)
(define-register esi reg32 6 #f)
(define-register edi reg32 7 #f)
(define-register ebp reg32 5 #f)
(define-register esp reg32 4 #f)
(define-register r8d reg32 8 #f)
(define-register r9d reg32 9 #f)
(define-register r10d reg32 10 #f)
(define-register r11d reg32 11 #f)
(define-register r12d reg32 12 #f)
(define-register r13d reg32 13 #f)
(define-register r14d reg32 14 #f)
(define-register r15d reg32 15 #f)

(define-register ah reg8 0 0)
(define-register bh reg8 3 0)
(define-register ch reg8 1 0)
(define-register dh reg8 2 0)
