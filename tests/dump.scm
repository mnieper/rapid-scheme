(program
 (entry proc)
 (module main
     (procedure proc
		(lea (0 magic) v0)
		(store (0 ptr) v0)
		(load (0 entry) r0)
		(dump filename r0)
		(halt))
   (procedure next
	      (load (0 ptr) v0)
	      (load (0 v0) v0)
	      (branch v0 42 (= ok))
	      (move 1 r0)
	      (call exit)
	      ok
	      (halt))   
   (datum filename "dump_obj.o")
   (variable entry next)
   (variable magic 42)
   (variable ptr 0)))
