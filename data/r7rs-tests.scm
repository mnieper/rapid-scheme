(import (scheme base)
	(scheme read)
	(scheme process-context)
	(rapid test)) ;; FIXME (srfi 64)

(test-begin "R7RS")

(test-equal 42
	    (read (open-input-string " 42 ")))

;; FIXME: Complete test suite.

(test-end)

(let ((runner (test-runner-current)))
  (exit (and (zero? (test-runner-fail-count runner))
	     (zero? (test-runner-xpass-count runner)))))

