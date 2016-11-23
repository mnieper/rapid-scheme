(program
 (entry (main proc))
 (module main
  	 (procedure proc
		    (halt))
	 (variable x 0))
 (init (main x) (main proc)))
