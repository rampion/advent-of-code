BIN:=solve/target/debug/solve
#BIN:=bin/Solve

run-all: run-example run-input

run-input: $(BIN)
	$< input

run-example: $(BIN)
	$< example

bin/Solve: Solve.hs bin/
	ghc --make $< -o $@

solve/target/debug/solve: solve/Cargo.toml solve/src/main.rs
	cargo build --manifest-path=$<

bin/:
	mkdir bin

solve/Cargo.toml:
	cargo new --vcs none solve

