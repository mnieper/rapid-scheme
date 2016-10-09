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
	(rename (rapid box test) (run-tests run-rapid-box-tests))
	(rename (rapid binary test) (run-tests run-rapid-binary-tests))
	(rename (rapid assembler test) (run-tests run-rapid-assembler-tests))
	(rename (rapid object-file test) (run-tests run-rapid-object-file-tests))
	(rename (rapid codegen test) (run-tests run-rapid-codegen-tests)))

(test-begin "Rapid Scheme")

(run-rapid-test-tests)
(run-rapid-box-tests)
(run-rapid-binary-tests)
(run-rapid-assembler-tests)
(run-rapid-object-file-tests)
(run-rapid-codegen-tests)

(test-end "Rapid Scheme")

(let ((runner (test-runner-current)))
  (exit (and (zero? (test-runner-fail-count runner))
	     (zero? (test-runner-xpass-count runner)))))
