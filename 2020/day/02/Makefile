BIN:=solve/target/debug/solve

run-all: run-example run-input

run-input: $(BIN)
	$< input

run-example: $(BIN)
	$< example

bin/Solve: Solve.hs
	ghc --make $< -o $@

solve/target/debug/solve: solve/Cargo.toml solve/src/main.rs
	cargo build --manifest-path=$<
