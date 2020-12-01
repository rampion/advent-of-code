run: bin/Solve
	./bin/Solve input

bin/Solve: bin Solve.hs
	ghc --make Solve.hs -o $@

bin:
	mkdir bin
