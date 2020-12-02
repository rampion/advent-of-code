run-all: run-example run-input

run-input: bin/Solve
	$< input

run-example: bin/Solve
	$< example

bin/Solve: Solve.hs
	ghc --make $< -o $@

