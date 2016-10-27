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
	(rename (rapid match test) (run-tests run-rapid-match-tests))
	(rename (rapid box test) (run-tests run-rapid-box-tests))
	(rename (rapid imap test) (run-tests run-rapid-imap-tests))
	(rename (rapid compiler identifier test) (run-tests run-rapid-compiler-identifier-tests))
	(rename (rapid binary test) (run-tests run-rapid-binary-tests))
	(rename (rapid compiler backend assembler test)
		(run-tests run-rapid-compiler-backend-assembler-tests))
	(rename (rapid compiler backend object-file test)
		(run-tests run-rapid-compiler-backend-object-file-tests))
	(rename (rapid compiler backend codegen test)
		(run-tests run-rapid-compiler-backend-codegen-tests)))

(test-begin "Rapid Scheme")

(run-rapid-test-tests)
(run-rapid-and-let-tests)
(run-rapid-match-tests)
(run-rapid-box-tests)
(run-rapid-imap-tests)
(run-rapid-compiler-identifier-tests)
(run-rapid-binary-tests)
(run-rapid-compiler-backend-assembler-tests)
(run-rapid-compiler-backend-object-file-tests)
(run-rapid-compiler-backend-codegen-tests)

(test-end "Rapid Scheme")

(let ((runner (test-runner-current)))
  (exit (and (zero? (test-runner-fail-count runner))
	     (zero? (test-runner-xpass-count runner)))))
