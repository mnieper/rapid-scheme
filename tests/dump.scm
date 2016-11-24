(program
 (entry (main proc))
 (module main
  	 (procedure proc
		    (dump filename proc)
		    (halt))
	 (datum filename "dump_obj.o")))
