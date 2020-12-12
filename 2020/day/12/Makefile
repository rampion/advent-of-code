HASKELL_BIN := dist-newstyle/bin/solve-aoc
RUST_BIN := target/debug/solve

BINS := ${HASKELL_BIN}
#${RUST_BIN}
INPUTS := $(wildcard data/*)

define NEWLINE


endef

run: $(BINS)
	$(foreach bin,$(BINS),$(bin) $(INPUTS)$(NEWLINE))

$(RUST_BIN): Cargo.toml src/main.rs
	cargo build

Cargo.toml src/Main.rs &:
	cargo init --name solve
	git add Cargo.toml src/main.rs

$(HASKELL_BIN): src/Main.hs solve-aoc.cabal | dist-newstyle/bin/
	cabal build
	find dist-newstyle/build -type f -name solve-aoc -exec cp {} dist-newstyle/bin/ \;

solve-aoc.cabal: | src/
	cabal init --package-name solve-aoc --synopsis "Solve Advent of Code day $(notdir $(PWD))" --license PublicDomain --main-is src/Main.hs --cabal-version '2.0'
	git add CHANGELOG.md Setup.hs solve-aoc.cabal src/Main.hs

src/ dist-newstyle/bin/:
	mkdir -p $@
