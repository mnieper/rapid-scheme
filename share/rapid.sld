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

(define-library (rapid)
  (export *
	  +
	  -
	  ...
	  /
	  <
	  <=
	  =
	  =>
	  >
	  >=
	  _
	  abs
	  and
	  append
	  apply
	  assoc
	  assq
	  assv
	  begin
	  binary-port?
	  boolean=?
	  boolean?
	  bytevector
	  bytevector-append
	  bytevector-copy
	  bytevector-copy!
	  bytevector-length
	  bytevector-u8-ref
	  bytevector-u8-set!
	  bytevector?
	  caar
	  cadr
	  (rename call/cc call-with-current-continuation)
	  call-with-input-file
	  call-with-output-file
	  call-with-port
	  call-with-values
	  call/cc
	  car
	  case
	  cdar
	  cddr
	  cdr
	  ceiling
	  char->integer
	  char-ready?
	  char<=?
	  char<?
	  char=?
	  char>=?
	  char>?
	  char?
	  close-input-port
	  close-output-port
	  close-port
	  complex?
	  cond
	  cond-expand
	  cons
	  current-error-port
	  current-input-port
	  current-output-port
	  define
	  define-record-type
	  define-syntax
	  define-values
	  delete-file
	  denominator
	  display
	  do
	  dynamic-wind
	  else
	  eof-object
	  eof-object?
	  eq?
	  equal?
	  eqv?
	  error
	  error-object-irritants
	  error-object-message
	  error-object?
	  even?
	  exact
	  exact-integer-sqrt
	  exact-integer?
	  exact?
	  expt
	  features
	  file-error?
	  file-exists?
	  floor
	  floor-quotient
	  floor-remainder
	  floor/
	  flush-output-port
	  for-each
	  gcd
	  get-output-bytevector
	  get-output-string
	  if
	  guard
	  include
	  include-ci
	  inexact
	  inexact?
	  input-port-open?
	  input-port?
	  integer->char
	  integer?
	  lambda
	  lcm
	  length
	  let
	  let*
	  let*-values
	  let-syntax
	  let-values
	  letrec
	  letrec*
	  letrec-syntax
	  list
	  list->string
	  list->vector
	  list-copy
	  list-ref
	  list-set!
	  list-tail
	  list?
	  make-bytevector
	  make-list
	  make-parameter
	  make-read-error
	  make-string
	  make-vector
	  map
	  max
	  member
	  memq
	  memv
	  min
	  modulo
	  negative?
	  newline
	  not
	  null?
	  number->string
	  number?
	  numerator
	  odd?
	  open-binary-input-file
	  open-binary-output-file
	  open-input-bytevector
	  open-input-file
	  open-input-string
	  open-output-bytevector
	  open-output-file
	  open-output-string
	  or
	  output-port-open?
	  output-port?
	  pair?
	  parameterize
	  peek-char
	  peek-u8
	  port?
	  port-ci?
	  port-set-ci?!
	  positive?
	  procedure?
	  quasiquote
	  quote
	  quotient
	  raise
	  raise-continuable
	  rational?
	  rationalize
	  read-bytevector
	  read-bytevector!
	  read-char
	  read-error?
	  read-line
	  read-string
	  read-u8
	  real?
	  remainder
	  reverse
	  round
	  set!
	  set-car!
	  set-cdr!
	  square
	  string
	  string->list
	  string->number
	  string->symbol
	  string->utf8
	  string->vector
	  string-append
	  string-copy
	  string-copy!
	  string-fill!
	  string-for-each
	  string-length
	  string-map
	  string-ref
	  string-set!
	  string<=?
	  string<?
	  string=?
	  string>=?
	  string>?
	  string?
	  substring
	  symbol->string
	  symbol=?
	  symbol?
	  syntax-error
	  syntax-rules
	  textual-port?
	  truncate
	  truncate-quotient
	  truncate-remainder
	  truncate/
	  u8-ready?
	  unless
	  unquote
	  unquote-splicing
	  utf8->string
	  values
	  vector
	  vector->list
	  vector->string
	  vector-append
	  vector-copy
	  vector-copy!
	  vector-fill!
	  vector-for-each
	  vector-length
	  vector-map
	  vector-ref
	  vector-set!
	  vector?
	  when
	  with-exception-handler
	  with-input-from-file
	  with-output-to-file
	  write
	  write-bytevector
	  write-char
	  write-simple
	  write-shared
	  write-string
	  write-u8
	  zero?

	  exit)
  (import (rapid primitive)
	  (rename (rapid primitives)
		  (binary-port? %binary-port?)
		  ;;(call/cc %call/cc)
		  (call-with-port %call-with-port)
		  (close-port %close-port)
		  (close-input-port %close-input-port)
		  (close-output-port %close-output-port)
		  (char-ready? %char-ready?)
		  (current-error-port %current-error-port)
		  (current-input-port %current-input-port)
		  (current-output-port %current-output-port)
		  (display %display)
		  ;;(error %error)
		  ;;(exit %exit)
		  (flush-output-port %flush-output-port)
		  (get-output-bytevector %get-output-bytevector)
		  (get-output-string %get-output-string)
		  (input-port? %input-port?)
		  (input-port-open? %input-port-open?)
		  (newline %newline)
		  (open-input-bytevector %open-input-bytevector)
		  (open-output-bytevector %open-output-bytevector)
		  (open-binary-input-file %open-binary-input-file)
		  (open-binary-output-file %open-binary-output-file)
		  (open-input-file %open-input-file)
		  (open-input-string %open-input-string)
		  (open-output-file %open-output-file)
		  (open-output-string %open-output-string)
		  (output-port? %output-port?)
		  (output-port-open? %output-port-open?)
		  (peek-char %peek-char)
		  (peek-u8 %peek-u8)
		  (port? %port?)
		  (read-bytevector %read-bytevector)
		  (read-bytevector! %read-bytevector!)
		  (read-char %read-char)
		  (read-line %read-line)
		  (read-string %read-string)
		  (read-u8 %read-u8)
		  (textual-port? %textual-port?)
		  (u8-ready? %u8-ready?)
		  (write %write)
		  (write-bytevector %write-bytevector)
		  (write-char %write-char)
		  (write-simple %write-simple)
		  (write-shared %write-shared)
		  (write-string %write-string)
		  (write-u8 %write-u8)))
  (include "rapid.scm"))
