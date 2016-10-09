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

(define-instruction (jmp imm)        #xE9 cd    )  ; XXX: imm->reloffset32
(define-instruction (movl imm reg32) #xB8 +rd id)
(define-instruction (nop)            #x90       )
(define-instruction (popq reg64)     #x58 +rq   )
(define-instruction (pushq reg64)    #x50 +rq   )
(define-instruction (ret)            #xC3       )
(define-instruction (ret imm)        #xC2 iw    )
