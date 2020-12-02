run: bin/Solve
	./bin/Solve input

bin/Solve_: bin Solve.hs
	ghc --make Solve.hs -o $@

bin:
	mkdir bin

bin/Solve: bin Solve.rs
	rustc Solve.rs -o $@
