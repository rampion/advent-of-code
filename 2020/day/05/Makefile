HASKELL_BIN := dist-newstyle/build/x86_64-linux/ghc-8.6.3/solve-aoc-0.1.0.0/x/solve-aoc/build/solve-aoc/solve-aoc
RUST_BIN := target/debug/solve

BIN := $(HASKELL_BIN)

run-all: run-example run-input

run-input: $(BIN)
	$< data/input

run-example: $(BIN)
	$< data/example

$(RUST_BIN): Cargo.toml src/main.rs
	cargo build

Cargo.toml:
	cargo init --name solve

$(HASKELL_BIN): solve-aoc.cabal src/Main.hs
	cabal build

solve-aoc.cabal:
	cabal init --package-name solve-aoc --synopsis "Solve Advent of Code day $(notdir $(PWD))" --license PublicDomain
