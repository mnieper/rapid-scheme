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

(define-primitive * '*)
(define-primitive + '+)
(define-primitive - '-)
(define-primitive / '/)
(define-primitive < '<)
(define-primitive <= '<=)
(define-primitive = '=)
(define-primitive > '>)
(define-primitive >= '>=)
(define-primitive abs 'abs)
(define-primitive append 'append)
(define-primitive apply 'apply)
(define-primitive assoc 'assoc)
(define-primitive assq 'assq)
(define-primitive assv 'assv)
(define-primitive binary-port? 'binary-port?)
(define-primitive boolean=? 'boolean=?)
(define-primitive boolean? 'boolean?)
(define-primitive bytevector 'bytevector)
(define-primitive bytevector-append 'bytevector-append)
(define-primitive bytevector-copy 'bytevector-copy)
(define-primitive bytevector-copy! 'bytevector-copy!)
(define-primitive bytevector-length 'bytevector-length)
(define-primitive bytevector-u8-ref 'bytevector-u8-ref)
(define-primitive bytevector-u8-set! 'bytevector-u8-set!)
(define-primitive bytevector? 'bytevector?)
(define-primitive caar 'caar)
(define-primitive cadr 'cadr)
(define-primitive call-with-port 'call-with-port)
(define-primitive call/cc 'call/cc)
(define-primitive car 'car)
(define-primitive cdar 'cdar)
(define-primitive cddr 'cddr)
(define-primitive cdr 'cdr)
(define-primitive ceiling 'ceiling)
(define-primitive char->integer 'char->integer)
(define-primitive char-ready? 'char-ready?)
(define-primitive char<=? 'char<=?)
(define-primitive char<? 'char<?)
(define-primitive char=? 'char=?)
(define-primitive char>=? 'char>=?)
(define-primitive char>? 'char>?)
(define-primitive char? 'char?)
(define-primitive close-input-port 'close-input-port)
(define-primitive close-output-port 'close-output-port)
(define-primitive close-port 'close-port)
(define-primitive complex? 'complex?)
(define-primitive cons 'cons)
(define-primitive current-error-port 'current-error-port)
(define-primitive current-input-port 'current-input-port)
(define-primitive current-output-port 'current-output-port)
(define-primitive denominator 'denominator)
(define-primitive dynamic-wind 'dynamic-wind)
(define-primitive eof-object 'eof-object)
(define-primitive eof-object? 'eof-object?)
(define-primitive eq? 'eq?)
(define-primitive equal? 'equal?)
(define-primitive eqv? 'eqv?)
(define-primitive error 'error)
(define-primitive error-object-irritants 'error-object-irritants)
(define-primitive error-object-message 'error-object-message)
(define-primitive error-object? 'error-object?)
(define-primitive even? 'even?)
(define-primitive exact 'exact)
(define-primitive exact-integer-sqrt 'exact-integer-sqrt)
(define-primitive exact-integer? 'exact-integer?)
(define-primitive exact? 'exact?)
(define-primitive expt 'expt)
(define-primitive file-error? 'file-error?)
(define-primitive floor 'floor)
(define-primitive floor-quotient 'floor-quotient)
(define-primitive floor-remainder 'floor-remainder)
(define-primitive floor/ 'floor/)
(define-primitive flush-output-port 'flush-output-port)
(define-primitive for-each 'for-each)
(define-primitive gcd 'gcd)
(define-primitive get-output-bytevector 'get-output-bytevector)
(define-primitive get-output-string 'get-output-string)
(define-primitive inexact 'inexact)
(define-primitive inexact? 'inexact?)
(define-primitive input-port-open? 'input-port-open?)
(define-primitive input-port? 'input-port?)
(define-primitive integer->char 'integer->char)
(define-primitive integer? 'integer?)
(define-primitive lcm 'lcm)
(define-primitive length 'length)
(define-primitive list 'list)
(define-primitive list->string 'list->string)
(define-primitive list->vector 'list->vector)
(define-primitive list-copy 'list-copy)
(define-primitive list-ref 'list-ref)
(define-primitive list-set! 'list-set!)
(define-primitive list-tail 'list-tail)
(define-primitive list? 'list?)
(define-primitive make-bytevector 'make-bytevector)
(define-primitive make-list 'make-list)
(define-primitive make-string 'make-string)
(define-primitive make-vector 'make-vector)
(define-primitive map 'map)
(define-primitive max 'max)
(define-primitive member 'member)
(define-primitive memq 'memq)
(define-primitive memv 'memv)
(define-primitive min 'min)
(define-primitive modulo 'modulo)
(define-primitive negative? 'negative?)
(define-primitive newline 'newline)
(define-primitive not 'not)
(define-primitive null? 'null?)
(define-primitive number->string 'number->string)
(define-primitive number? 'number?)
(define-primitive numerator 'numerator)
(define-primitive odd? 'odd?)
(define-primitive open-input-bytevector 'open-input-bytevector)
(define-primitive open-input-string 'open-input-string)
(define-primitive open-output-bytevector 'open-output-bytevector)
(define-primitive open-output-string 'open-output-string)
(define-primitive output-port-open? 'output-port-open?)
(define-primitive output-port? 'output-port?)
(define-primitive pair? 'pair?)
(define-primitive peek-char 'peek-char)
(define-primitive peek-u8 'peek-u8)
(define-primitive port? 'port?)
(define-primitive positive? 'positive?)
(define-primitive procedure? 'procedure?)
(define-primitive quotient 'quotient)
(define-primitive raise 'raise)
(define-primitive raise-continuable 'raise-continuable)
(define-primitive rational? 'rational?)
(define-primitive rationalize 'rationalize)
(define-primitive read-bytevector 'read-bytevector)
(define-primitive read-bytevector! 'read-bytevector!)
(define-primitive read-char 'read-char)
(define-primitive read-line 'read-line)
(define-primitive read-string 'read-string)
(define-primitive read-u8 'read-u8)
(define-primitive real? 'real?)
(define-primitive remainder 'remainder)
(define-primitive reverse 'reverse)
(define-primitive round 'round)
(define-primitive set-car! 'set-car!)
(define-primitive set-cdr! 'set-cdr!)
(define-primitive square 'square)
(define-primitive string 'string)
(define-primitive string->list 'string->list)
(define-primitive string->number 'string->number)
(define-primitive string->symbol 'string->symbol)
(define-primitive string->utf8 'string->utf8)
(define-primitive string->vector 'string->vector)
(define-primitive string-append 'string-append)
(define-primitive string-copy 'string-copy)
(define-primitive string-copy! 'string-copy!)
(define-primitive string-fill! 'string-fill!)
(define-primitive string-for-each 'string-for-each)
(define-primitive string-length 'string-length)
(define-primitive string-map 'string-map)
(define-primitive string-ref 'string-ref)
(define-primitive string-set! 'string-set!)
(define-primitive string<=? 'string<=?)
(define-primitive string<? 'string<?)
(define-primitive string=? 'string=?)
(define-primitive string>=? 'string>=?)
(define-primitive string>? 'string>?)
(define-primitive string<? 'string<?)
(define-primitive string=? 'string=?)
(define-primitive string>=? 'string>=?)
(define-primitive string>? 'string>?)
(define-primitive string? 'string?)
(define-primitive substring 'substring)
(define-primitive symbol->string 'symbol->string)
(define-primitive symbol=? 'symbol=?)
(define-primitive symbol? 'symbol?)
(define-primitive textual-port? 'textual-port?)
(define-primitive truncate 'truncate)
(define-primitive truncate-quotient 'truncate-quotient)
(define-primitive truncate-remainder 'truncate-remainder)
(define-primitive truncate/ 'truncate/)
(define-primitive u8-ready? 'u8-ready?)
(define-primitive utf8->string 'utf8->string)
(define-primitive vector 'vector)
(define-primitive vector->list 'vector->list)
(define-primitive vector->string 'vector->string)
(define-primitive vector-append 'vector-append)
(define-primitive vector-copy 'vector-copy)
(define-primitive vector-copy! 'vector-copy!)
(define-primitive vector-fill! 'vector-fill!)
(define-primitive vector-for-each 'vector-for-each)
(define-primitive vector-length 'vector-length)
(define-primitive vector-map 'vector-map)
(define-primitive vector-ref 'vector-ref)
(define-primitive vector-set! 'vector-set!)
(define-primitive vector? 'vector?)
(define-primitive with-exception-handler 'with-exception-handler)
(define-primitive write-bytevector 'write-bytevector)
(define-primitive write-char 'write-char)
(define-primitive write-string 'write-string)
(define-primitive write-u8 'write-u8)
(define-primitive zero? 'zero?)

(define-primitive display 'display)
(define-primitive write 'write)
(define-primitive write-shared 'write-shared)
(define-primitive write-simple 'write-simple)

(define-primitive caaar 'caaar)
(define-primitive caadr 'caadr)
(define-primitive cadar 'cadar)
(define-primitive caddr 'caddr)
(define-primitive cdaar 'cdaar)
(define-primitive cdadr 'cdadr)
(define-primitive cddar 'cddar)
(define-primitive cdddr 'cdddr)
(define-primitive caaaar 'caaaar)
(define-primitive caaadr 'caaadr)
(define-primitive caadar 'caadar)
(define-primitive caaddr 'caaddr)
(define-primitive cadaar 'cadaar)
(define-primitive cadadr 'cadadr)
(define-primitive caddar 'caddar)
(define-primitive cadddr 'cadddr)
(define-primitive cdaaar 'cdaaar)
(define-primitive cdaadr 'cdaadr)
(define-primitive cdadar 'cdadar)
(define-primitive cdaddr 'cdaddr)
(define-primitive cddaar 'cddaar)
(define-primitive cddadr 'cddadr)
(define-primitive cdddar 'cdddar)
(define-primitive cddddr 'cddddr)

(define-primitive delete-file 'delete-file)
(define-primitive file-exists? 'file-exists?)
(define-primitive open-binary-input-file 'open-binary-input-file)
(define-primitive open-binary-output-file 'open-binary-output-file)
(define-primitive open-input-file 'open-input-file)
(define-primitive open-output-file 'open-output-file)

(define-primitive interaction-environment 'interaction-environment)

(define-primitive null-environment 'null-environment)
(define-primitive scheme-report-environment 'scheme-report-environment)

(define-primitive acos 'acos)
(define-primitive asin 'asin)
(define-primitive atan 'atan)
(define-primitive cos 'cos)
(define-primitive exp 'exp)
(define-primitive finite? 'finite?)
(define-primitive infinite? 'infinite?)
(define-primitive log 'log)
(define-primitive nan? 'nan?)
(define-primitive sin 'sin)
(define-primitive sqrt 'sqrt)
(define-primitive tan 'tan)

(define-primitive char-alphabetic? 'char-alphabetic)
(define-primitive char-ci<=? 'char-ci<=?)
(define-primitive char-ci<? 'char-ci<?)
(define-primitive char-ci=? 'char-ci=?)
(define-primitive char-ci>=? 'char-ci>=?)
(define-primitive char-ci>? 'char-ci>?)
(define-primitive char-downcase 'char-downcase)
(define-primitive char-foldcase 'char-foldcase)
(define-primitive char-lower-case? 'char-lower-case?)
(define-primitive char-numeric? 'char-numeric?)
(define-primitive char-upcase 'char-upcase)
(define-primitive char-upper-case? 'char-upper-case?)
(define-primitive char-whitespace? 'char-whitespace?)
(define-primitive digit-value 'digit-value)
(define-primitive string-ci<=? 'string-ci<=?)
(define-primitive string-ci<? 'string-ci<?)
(define-primitive string-ci=? 'string-ci=?)
(define-primitive string-ci>=? 'string-ci>=?)
(define-primitive string-ci>? 'string-ci>?)
(define-primitive string-downcase 'string-downcase)
(define-primitive string-foldcase 'string-foldcase)
(define-primitive string-upcase 'string-upcase)

(define-primitive current-jiffy 'current-jiffy)
(define-primitive current-second 'current-second)
(define-primitive jiffies-per-second 'jiffies-per-second)

(define-primitive command-line 'command-line)
(define-primitive emergency-exit 'emergency-exit)
(define-primitive exit 'exit)
(define-primitive get-environment-variable 'get-environment-variable)
(define-primitive get-environment-variables 'get-environment-variables)

(define-primitive angle 'angle)
(define-primitive imag-part 'imag-part)
(define-primitive magnitude 'magnitude)
(define-primitive make-polar 'make-polar)
(define-primitive make-rectangular 'make-rectangular)
(define-primitive real-part 'real-part)

(define-primitive eval 'eval)
(define-primitive environment 'environment)

(define-primitive load 'load)

(define-primitive make-type 'make-type)
(define-primitive make-instance 'make-instance)
(define-primitive instance? 'instance?)
(define-primitive instance-ref 'instance-ref)
