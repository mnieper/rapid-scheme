(define-library (lib)
  (export define
	  make-record bar bar? set-bar!
	  foo)
  (import (rapid primitive))
  (include "lib.scm"))
