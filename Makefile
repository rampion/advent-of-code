DIR := $(shell date +"%Y/day/%-d")
SRC = $(DIR)/$(PART).hs
EXE = $(shell date +"day%-d")-$(PART)

export define HASKELL_TEMPLATE =
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = return ()

test :: IO ()
test = doctest ["$(SRC)"]
endef

export define CABAL_TEMPLATE =

executable $(EXE)
  main-is:             $(SRC)
  build-depends:       base, doctest
  default-language:    Haskell2010
endef

default: part.1

part.1: PART=part1
part.2: PART=part2

part.%: $(DIR)/part%.hs
	make --quiet ghcid EXE=$(EXE)

$(DIR):
	mkdir -p $(DIR)

$(DIR)/part%.hs: $(DIR)
	grep "executable $(EXE)" advent-of-code.cabal || echo "$$CABAL_TEMPLATE" >> advent-of-code.cabal
	echo "$$HASKELL_TEMPLATE" > $@

ghcid:
	ghcid --command "cabal v2-repl $(EXE)" --restart=advent-of-code.cabal --run --test=test --allow-eval

.PRECIOUS: $(DIR)/part%.hs
.PHONY: default part.% ghcid update-cabal
