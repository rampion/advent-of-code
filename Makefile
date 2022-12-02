run: advent-of-code.cabal
	cabal run advent-of-code -- "$(day)" $(part)

ghcid-test: advent-of-code.cabal
	ghcid --target test --run

advent-of-code.cabal: package.yaml
	hpack --force .
