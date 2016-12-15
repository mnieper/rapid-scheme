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
	(scheme process-context)
	(rapid test)
	(rename (rapid test test) (run-tests run-rapid-test-tests))
	(rename (rapid and-let test) (run-tests run-rapid-and-let-tests))
	(rename (rapid receive test) (run-tests run-rapid-receive-tests))
	(rename (rapid box test) (run-tests run-rapid-box-tests))
	(rename (rapid generator test) (run-tests run-rapid-generator-tests))
	(rename (rapid comparator test) (run-tests run-rapid-comparator-tests))
	(rename (rapid set test) (run-tests run-rapid-set-tests))
	(rename (rapid map test) (run-tests run-rapid-map-tests))
	(rename (rapid digraph test)
		(run-tests run-rapid-digraph-tests))
	(rename (rapid sort test) (run-tests run-rapid-sort-tests))
	(rename (rapid list test) (run-tests run-rapid-list-tests))
	(rename (rapid iset test) (run-tests run-rapid-iset-tests))
	(rename (rapid imap test) (run-tests run-rapid-imap-tests))
	(rename (rapid graph test) (run-tests run-rapid-graph-tests))
	(rename (rapid match test) (run-tests run-rapid-match-tests))
	(rename (rapid compiler identifier test) (run-tests run-rapid-compiler-identifier-tests))
	(rename (rapid compiler syntax test) (run-tests run-rapid-compiler-syntax-tests))
	(rename (rapid binary test) (run-tests run-rapid-binary-tests))
	(rename (rapid compiler free-variables test)
		(run-tests run-rapid-compiler-free-variables-tests))
	(rename (rapid compiler partition-bindings test)
		(run-tests run-rapid-compiler-partition-bindings-tests))
	(rename (rapid compiler mark-escaping-procedures test)
		(run-tests run-rapid-compiler-mark-escaping-procedures-tests))
	(rename (rapid compiler closure-convert test)
		(run-tests run-rapid-compiler-closure-convert-tests))	
	(rename (rapid compiler parallel-move test)
		(run-tests run-rapid-compiler-parallel-move-tests))	
	(rename (rapid compiler generate-module test)
		(run-tests run-rapid-compiler-generate-module-tests))
	(rename (rapid compiler backend assembler test)
		(run-tests run-rapid-compiler-backend-assembler-tests))
	(rename (rapid compiler backend object-file test)
		(run-tests run-rapid-compiler-backend-object-file-tests))
	(rename (rapid compiler backend codegen test)
		(run-tests run-rapid-compiler-backend-codegen-tests)))

(test-begin "Rapid Scheme")

(run-rapid-test-tests)
(run-rapid-and-let-tests)
(run-rapid-receive-tests)
(run-rapid-box-tests)
(run-rapid-generator-tests)
(run-rapid-comparator-tests)
(run-rapid-set-tests)
(run-rapid-map-tests)
(run-rapid-digraph-tests)
(run-rapid-sort-tests)
(run-rapid-list-tests)
(run-rapid-iset-tests)
(run-rapid-imap-tests)
(run-rapid-graph-tests)
(run-rapid-match-tests)
(run-rapid-compiler-syntax-tests)
(run-rapid-compiler-identifier-tests)
(run-rapid-binary-tests)
(run-rapid-compiler-free-variables-tests)
(run-rapid-compiler-partition-bindings-tests)
(run-rapid-compiler-mark-escaping-procedures-tests)
(run-rapid-compiler-closure-convert-tests)
(run-rapid-compiler-parallel-move-tests)
(run-rapid-compiler-generate-module-tests)
(run-rapid-compiler-backend-assembler-tests)
(run-rapid-compiler-backend-object-file-tests)
(run-rapid-compiler-backend-codegen-tests)

(test-end "Rapid Scheme")

(let ((runner (test-runner-current)))
  (exit (and (zero? (test-runner-fail-count runner))
	     (zero? (test-runner-xpass-count runner)))))
