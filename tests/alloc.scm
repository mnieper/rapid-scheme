(program
 (entry main)
 (module alloc
     (procedure main
		(move 100000 r0)
		loop
		(branch r0 0 (= exit))
		(alloc 1 20 r0)
		(record 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 r1)
		(add -1 r0 r0)
		(jump loop)
		exit
		(halt))))
