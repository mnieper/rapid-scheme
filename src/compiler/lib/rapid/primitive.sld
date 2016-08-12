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

(define-library (rapid primitive)
  (export case-lambda
	  if
	  begin
	  quote
	  letrec
	  
	  make-rtd
	  make-constructor
	  make-predicate
	  make-accessor
	  make-mutator
	  
	  make-cell
	  cell-ref
	  cell-set!
	  
	  *
	  +
	  -
	  /
	  <
	  <=
	  =
	  >
	  >=
	  abs
	  append
	  apply
	  assoc
	  assq
	  assv
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
	  call-with-port	  
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
	  denominator
	  dynamic-wind
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
	  floor
	  floor-quotient
	  floor-remainder
	  floor/
	  flush-output-port
	  for-each
	  gcd
	  get-output-bytevector
	  get-output-string
	  inexact
	  inexact?
	  input-port-open?
	  input-port?
	  integer->char
	  integer?
	  lcm
	  length
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
	  open-input-bytevector
	  open-input-string
	  open-output-bytevector
	  open-output-string
	  output-port-open?
	  output-port?
	  pair?
	  peek-char
	  peek-u8
	  port?
	  positive?
	  procedure?
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
	  string=?
	  string>=?
	  string>?
	  string<?
	  string?
	  substring
	  symbol->string
	  symbol=?
	  symbol?
	  textual-port?
	  truncate
	  truncate-quotient
	  truncate-remainder
	  truncate/
	  u8-ready?
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
	  with-exception-handler
	  write-bytevector
	  write-char
	  write-string
	  write-u8
	  zero?

	  char-alphabetic?
	  char-ci<=?
	  char-ci<?
	  char-ci=?
	  char-ci>=?
	  char-ci>?
	  char-downcase
	  char-foldcase
	  char-lower-case?
	  char-numeric?
	  char-upcase
	  char-upper-case?
	  char-whitespace?
	  digit-value
	  string-ci<=?
	  string-ci<?
	  string-ci=?
	  string-ci>=?
	  string-ci>?
	  string-downcase
	  string-foldcase
	  string-upcase

	  angle
	  magnitude
	  make-rectangular
	  imag-part
	  make-polar
	  real-part
	  
	  caaar
	  caadr
	  cadar
	  caddr
	  cdaar
	  cdadr
	  cddar
	  cdddr
	  caaaar
	  caaadr
	  caadar
	  caaddr
	  cadaar
	  cadadr
	  caddar
	  cadddr
	  cdaaar
	  cdaadr
	  cdadar
	  cdaddr
	  cddaar
	  cddadr
	  cdddar
	  cddddr

	  environment
	  eval

	  delete-file
	  file-exists?
	  open-binary-input-file
	  open-binary-output-file
	  open-input-file
	  open-output-file

	  acos
	  asin
	  atan
	  cos
	  exp
	  finite?
	  infinite?
	  log
	  nan?
	  sin
	  sqrt
	  tan
	  
	  load

	  command-line
	  emergency-exit
	  exit
	  get-environment-variable
	  get-environment-variables

	  read

	  interaction-environment

	  current-jiffy
	  current-second
	  jiffies-per-second

	  display
	  write
	  write-shared
	  write-simple

	  null-environment
	  scheme-report-environment)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme char)
	  (scheme complex)
	  (scheme cxr)
	  (scheme eval)
	  (scheme file)
	  (scheme inexact)
	  (scheme load)
	  (scheme process-context)
	  (scheme read)
	  (scheme repl)
	  (scheme time)
	  (scheme write)
	  (scheme r5rs))
  (include "primitive.scm"))
