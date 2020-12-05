BIN:=target/debug/solve
#BIN:=bin/Solve

run-all: run-example run-input

run-input: $(BIN)
	$< data/input

run-example: $(BIN)
	$< data/example

bin/Solve: Solve.hs bin/
	ghc --make $< -o $@ -package optparse-generic

target/debug/solve: Cargo.toml src/main.rs
	cargo build

bin/:
	mkdir bin

Cargo.toml:
	cargo init --name solve

