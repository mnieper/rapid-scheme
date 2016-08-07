(import (scheme base)
	(scheme read)
	(scheme process-context)
	(rapid test)) ;; FIXME (srfi 64)

(test-begin "R7RS")

(test-begin "Records")

(let ()
  (define-record-type <pare>
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))

  (test-assert (pare? (kons 1 2)))
  (test-assert (not (pare? (cons 1 2))))
  (test-equal 1 (kar (kons 1 2)))
  (test-equal 2 (kdr (kons 1 2)))
  (test-equal 3 (let ((k (kons 1 2)))
		  (set-kar! k 3)
		  (kar k))))

(test-end "Records")

(test-begin "Input")

(test-equal 42
	    (read (open-input-string " 42 ")))

(test-end "Input")

;; FIXME: Complete test suite.

(test-end)

(let ((runner (test-runner-current)))
  (exit (and (zero? (test-runner-fail-count runner))
	     (zero? (test-runner-xpass-count runner)))))

