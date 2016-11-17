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

(define-instruction (addq imm32 reg/mem64) rex #x81 / 0 id)
(define-instruction (subq imm32 reg/mem64) rex #x81 / 5 id)
(define-instruction (byte imm8)            ib         )
(define-instruction (callq reg/mem64)      #xFF / 2   )
(define-instruction (cmpq reg/mem64 reg64) rex #x3B /r)
(define-instruction (cmpq imm32 reg/mem64) rex #x81 / 7 id)
(define-instruction (jmpq reg/mem64)       #xFF / 4   )
(define-instruction (je rel32off)          #x0F #x84 cd)
(define-instruction (jmp rel32off)         #xE9 cd    )
(define-instruction (leaq mem reg64)   rex #x8D /r    )
(define-instruction (movl imm32 reg32)     #xB8 +rd id)
(define-instruction (movq imm64 reg64) rex #xB8 +rq iq)
(define-instruction (movq reg/mem64 reg64) rex #x8B /r)
(define-instruction (movq reg64 reg/mem64) rex #x89 /r)
(define-instruction (nop)                  #x90       )
(define-instruction (popq reg64)           #x58 +rq   )
(define-instruction (pushq reg64)          #x50 +rq   )
(define-instruction (pushq imm64)          #x68       )
(define-instruction (quad imm64)           iq         )
(define-instruction (ret)                  #xC3       )
(define-instruction (ret imm16)            #xC2 iw    )
(define-instruction (sarq reg/mem64)   rex #xD1 / 7   )
