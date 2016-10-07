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

(define-library (rapid binary test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid binary))
  (begin
    (define (run-tests)
      (test-begin "Rapid Binary")

      (test-equal "Encoding integers as bytevectors"
	#u8(#xCD #x12 #xAB #x90)
	(integer->bytevector #x90AB12CD 4))

      (test-equal "Encoding negative integers as bytevectors"
	#u8(#xFE #xFF)
	(integer->bytevector -2 2))

      (test-equal "Decoding bytevectors as integers"
	#x90AB12CD
	(bytevector->integer #u8(#xCD #x12 #xAB #x90)))

      (test-equal "Saving integers in bytevectors"
	#u8(#xAA #xBB #xCC)
	(let ((bv (bytevector #xAA #x00 #x00)))
	  (bytevector-integer-set! bv 1 #xCCBB 2)
	  bv))

      (test-equal "Retrieving integers from bytevectors"
	#xFF02
	(bytevector-integer-ref #u8(#xAA #x02 #xFF) 1 2))
	   
      (test-end))))
