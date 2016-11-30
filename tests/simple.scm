(program
 (entry (main proc))
 (module main
  	 (procedure proc
		    (jump (x)))
	 (procedure next
		    (halt))
	 (variable x 0))
 (init (main x) (main next)))
