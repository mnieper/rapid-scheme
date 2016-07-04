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

;; Copyright (C) John Cowan, Will Clinger (2015). All Rights Reserved.
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax define-record-type
  (syntax-rules ()
    ((_ (type-name parent) constructor-spec predicate-spec . field-specs)
     (define-record-type-helper0
       type-name parent constructor-spec predicate-spec . field-specs))
    ((_ type-name constructor-spec predicate-spec . field-specs)
     (define-record-type-helper0
       type-name #f constructor-spec predicate-spec . field-specs))))

(define-syntax define-record-type-helper0
  (syntax-rules ()
    ((_ type-name parent constructor-spec predicate-spec . field-specs)
     (define-record-type-helper1
       type-name parent constructor-spec predicate-spec field-specs ()))))

(define-syntax define-record-type-helper1
  (syntax-rules ()
    ((_ type-name parent constructor-spec predicate-spec () rev-specs)
     (define-record-type-helper2
       type-name parent constructor-spec predicate-spec rev-specs () () ()))
    ((_ type-name parent constructor-spec predicate-spec
	(spec . field-specs) rev-specs)
     (define-record-type-helper1
       type-name parent constructor-spec predicate-spec
       field-specs (spec . rev-specs)))))

(define-syntax define-record-type-helper2
  (syntax-rules ()
    ((_ type-name
	parent constructor-spec predicate-spec
	() accessors mutators fields)
     (define-record-type-helper3
       type-name fields
       parent constructor-spec predicate-spec accessors mutators))
    ((_ type-name parent constructor-spec predicate-spec
	((field-name accessor-name) . field-specs)
	accessors mutators fields)
     (define-record-type-helper2
       type-name parent constructor-spec predicate-spec
       field-specs
       ((accessor-name field-name) . accessors)
       mutators
       (field-name . fields)))
    ((_ type-name parent constructor-spec predicate-spec
	((field-name accessor-name mutator-name) . field-specs)
	accessors mutators fields)
     (define-record-type-helper2
       type-name parent constructor-spec predicate-spec
       field-specs
       ((accessor-name field-name) . accessors)
       ((mutator-name field-name) . mutators)
       (field-name . fields)))))

(define-syntax define-record-type-helper3
  (syntax-rules ()
    ((_ type-name fields parent #f predicate
	((accessor field) ...) ((mutator mutable-field) ...))
     (define-record-type-helper3
       type-name fields parent ignored predicate
       ((accessor field) ...) ((mutator mutable-field) ...)))    
    ((_ type-name fields parent constructor #f
         ((accessor field) ...) ((mutator mutable-field) ...))
     (define-record-type-helper3
       type-name fields parent constructor ignored
       ((accessor field) ...) ((mutator mutable-field) ...)))    
    ((_ type-name (field1 ...) parent (constructor arg ...) predicate
	((accessor field) ...) ((mutator mutable-field) ...))
     (begin (define type-name (make-rtd 'type-name #(field1 ...) parent))
	    (define constructor (rtd-constructor type-name #(arg ...)))
	    (define predicate (rtd-predicate type-name))
	    (define accessor (rtd-accessor type-name 'field))
	    ...
	    (define mutator (rtd-mutator type-name 'mutable-field))
	    ...))
    ((_ type-name (field1 ...) parent constructor predicate
	((accessor field) ...) ((mutator mutable-field) ...))
     (begin (define type-name (make-rtd 'type-name #(field1 ...) parent))
	    (define constructor (rtd-constructor type-name #()))
	    (define predicate (rtd-predicate type-name))
	    (define accessor (rtd-accessor type-name 'field))
	    ...
	    (define mutator (rtd-mutator type-name 'mutable-field))
	    ...))))

(scheme-define-record-type <rtd>
  (%make-rtd name fieldspecs size ancestors)
  rtd?
  (name rtd-name)
  (fieldspecs rtd-fieldspecs)
  (size rtd-size)
  (ancestors rtd-ancestors rtd-set-ancestors!))

(scheme-define-record-type <record>
  (make-record rtd fields)
  record?
  (rtd record-rtd)
  (fields record-fields))

(define field-comparator
  (make-comparator symbol? symbol=?
		   (lambda (x y)
		     (string<? (symbol->string x) (symbol->string y)))
		   #f))

(define root (%make-rtd '<> (imap field-comparator) 0 (iset field-comparator)))

(define make-rtd
  (case-lambda
   ((name fieldspecs)
    (make-rtd name fieldspecs #f))
   ((name fieldspecs parent)
    (let*
	((parent (or parent root))
	 (fieldspecs+size
	  (vector-fold
	   (lambda (state fieldspec)
	     (let ((size (vector-ref state 1)))
	       (vector (imap-replace (vector-ref state 0) fieldspec size)
		       (+ size 1))))
	   (vector (rtd-fieldspecs parent) (rtd-size parent))
	   fieldspecs)))
      (let ((rtd (%make-rtd name
		            (vector-ref fieldspecs+size 0)
			    (vector-ref fieldspecs+size 1)
			    #f)))
	(rtd-set-ancestors! rtd (iset-adjoin (rtd-ancestors parent) rtd))
	rtd)))))

(define (rtd-constructor rtd fieldspecs)
  (let* ((fieldspec-map (rtd-fieldspecs rtd))
	 (size (rtd-size rtd))
	 (indexes
	  (map (lambda (fieldspec)
		 (imap-ref fieldspec-map fieldspec))
	       (vector->list fieldspecs))))
    (lambda args
      (unless (= (length args) (length indexes))
	(error "not enough arguments" (length args)))
      (let ((fields (make-vector size)))
	(for-each (lambda (index arg)
		    (vector-set! fields index arg))
		  indexes args)
	(make-record rtd fields)))))

(define (rtd-predicate rtd)
  (lambda (obj)
    ;; FIXME: rtd-ancestors is not a set of symbols, which the comparator assumes.
    (and (record? obj) (iset-member? (rtd-ancestors (record-rtd obj)) rtd))))

(define (rtd-accessor rtd field)
  (let ((index (imap-ref (rtd-fieldspecs rtd) field)))
    (lambda (record)
      ;; FIXME: Better error handling for records
      (unless (record? record)
	(error "not a record" record))
      (unless (iset-member? (rtd-ancestors (record-rtd record)) rtd)
	(error "record type-mismatch" (rtd-name (record-rtd record))
	       (rtd-name rtd)))
      (vector-ref (record-fields record) index))))

(define (rtd-mutator rtd field)
  (let ((index (imap-ref (rtd-fieldspecs rtd) field)))
    (lambda (record value)
      (vector-set! (record-fields record) index value))))
