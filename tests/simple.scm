(program
 (entry proc)
 (module main
  	 (procedure proc
		    (jump (x)))
	 (procedure next
		    (halt))
	 (variable x next)))
