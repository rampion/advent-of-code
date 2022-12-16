ifndef part
  part := 1
endif

run: advent-of-code.cabal
	cabal run advent-of-code -- "$(day)" Part$(part)

ghcid: advent-of-code.cabal
	ghcid --target advent-of-code

ghcid-test: advent-of-code.cabal
	ghcid --target test --run

advent-of-code.cabal: package.yaml $(wildcard src/*.hs app/*.hs test/*.hs)
	hpack --force .
