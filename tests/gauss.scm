(program
 (entry main)
 (module gauss
	 (procedure main
		    (move 0 v0)
		    (move 3 v1)
		    loop1
		    (branch v1 0 (= break))
		    (add v0 v1 v0)
		    (add -1 v1 v1)
		    (jump loop1)
		    break
		    (branch v0 6 (= ok))
		    (move 1 r0)
		    (call exit)
		    ok
		    (halt))))
