ifdef ghcid
run: ghcid
endif

today:
	mkdir -p $$(date +"%Y/day/%-d")

ghcid:
	ghcid --command "cabal repl $(ghcid)" --restart=advent-of-code.cabal --run

.PHONY: today ghcid
