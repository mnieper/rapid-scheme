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

(import
 (scheme base)
 (scheme process-context)
 (rapid test)
 (rename (rapid test test) (run-tests run-rapid-test-tests))
 (rename (rapid and-let test) (run-tests run-rapid-and-let-tests))
 (rename (rapid receive test) (run-tests run-rapid-receive-tests))
 (rename (rapid vectors test) (run-tests run-rapid-vectors-tests))
 (rename (rapid records test) (run-tests run-rapid-records-tests))
 (rename (rapid args-fold test) (run-tests run-rapid-args-fold-tests))
 (rename (rapid boxes test) (run-tests run-rapid-boxes-tests))
 (rename (rapid generators test) (run-tests run-rapid-generators-tests))
 (rename (rapid comparators test) (run-tests run-rapid-comparators-tests))
 (rename (rapid format test) (run-tests run-rapid-format-tests))
 (rename (rapid list-queues test) (run-tests run-rapid-list-queues-tests))
 (rename (rapid immutable-sets test) (run-tests run-rapid-immutable-sets-tests))
 (rename (rapid immutable-maps test) (run-tests run-rapid-immutable-maps-tests))
 (rename (rapid lists test) (run-tests run-rapid-lists-tests))
 (rename (rapid graphs test) (run-tests run-rapid-graphs-tests))
 (rename (rapid parameter-objects test)
	 (run-tests run-rapid-parameter-objects-tests))
 (rename (rapid features test) (run-tests run-rapid-features-tests))
 (rename (rapid paths test) (run-tests run-rapid-paths-tests))
 (rename (rapid identifiers test) (run-tests run-rapid-identifiers-tests))
 (rename (rapid syntax test) (run-tests run-rapid-syntax-tests))
 (rename (rapid read test) (run-tests run-rapid-read-tests))
 (rename (rapid syntactic-environments test)
	 (run-tests run-rapid-syntactic-environments-tests))
 (rename (rapid import-sets test) (run-tests run-rapid-import-sets-tests))
 (rename (rapid library-definitions test)
	 (run-tests run-rapid-library-definitions-tests))
 (rename (rapid expressions test) (run-tests run-rapid-expressions-test))
 (rename (rapid expand test) (run-tests run-rapid-expand-tests))
 (rename (rapid macro-transformers test)
	 (run-tests run-rapid-macro-transformers-tests))
 (rename (rapid primitive-environment test)
	 (run-tests run-rapid-primitive-environment-tests))
 (rename (rapid expand-library test)
	 (run-tests run-rapid-expand-library-tests))
 (rename (rapid bind-procedures test)
	 (run-tests run-rapid-bind-procedures-tests))
 (rename (rapid fix-letrec test) (run-tests run-rapid-fix-letrec-tests))
 (rename (rapid cps-transform test) (run-tests run-rapid-cps-transform-tests))
 (rename (rapid mutable-variable-eliminate test)
	 (run-tests run-rapid-mutable-variable-eliminate-tests))
 (rename (rapid introduce-let test) (run-tests run-rapid-introduce-let-tests)))

(test-begin "Rapid Scheme")

(run-rapid-test-tests)
(run-rapid-and-let-tests)
(run-rapid-receive-tests)
(run-rapid-vectors-tests)
(run-rapid-records-tests)
(run-rapid-args-fold-tests)
(run-rapid-boxes-tests)
(run-rapid-generators-tests)
(run-rapid-comparators-tests)
(run-rapid-format-tests)
(run-rapid-list-queues-tests)
(run-rapid-immutable-maps-tests)
(run-rapid-immutable-sets-tests)
(run-rapid-lists-tests)
(run-rapid-graphs-tests)
(run-rapid-parameter-objects-tests)
(run-rapid-features-tests)
(run-rapid-paths-tests)
(run-rapid-identifiers-tests)
(run-rapid-syntax-tests)
(run-rapid-read-tests)
(run-rapid-import-sets-tests)
(run-rapid-library-definitions-tests)
(run-rapid-syntactic-environments-tests)
(run-rapid-expressions-test)
(run-rapid-expand-tests)
(run-rapid-macro-transformers-tests)
(run-rapid-primitive-environment-tests)
(run-rapid-bind-procedures-tests)
(run-rapid-fix-letrec-tests)
(run-rapid-cps-transform-tests)
(run-rapid-mutable-variable-eliminate-tests)
(run-rapid-introduce-let-tests)
(run-rapid-expand-library-tests)

(test-end "Rapid Scheme")

(let ((runner (test-runner-current)))
  (exit (and (zero? (test-runner-fail-count runner))
	     (zero? (test-runner-xpass-count runner)))))
