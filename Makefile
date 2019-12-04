ifdef ghcid
run: ghcid
endif

today:
	mkdir -p $$(date +"%Y/day/%-d")

ghcid:
	ghcid --command "cabal repl $(ghcid)" 

.PHONY: today ghcid
