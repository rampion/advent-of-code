HASKELL_BIN := dist-newstyle/build/x86_64-linux/ghc-8.6.3/solve-aoc-0.1.0.0/x/solve-aoc/build/solve-aoc/solve-aoc
RUST_BIN := target/debug/solve

BINS := ${HASKELL_BIN} ${RUST_BIN}
INPUTS := $(wildcard data/*)

define NEWLINE


endef

run: $(BINS)
	$(foreach bin,$(BINS),$(bin) $(INPUTS)$(NEWLINE))

$(RUST_BIN): Cargo.toml src/main.rs
	cargo build

Cargo.toml:
	cargo init --name solve

$(HASKELL_BIN): src/Main.hs solve-aoc.cabal
	cabal build

 solve-aoc.cabal src/Main.hs &: | src
	cabal init --package-name solve-aoc --synopsis "Solve Advent of Code day $(notdir $(PWD))" --license PublicDomain --main-is src/Main.hs --cabal-version '2.0'

src:
	mkdir $@
