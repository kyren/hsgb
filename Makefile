build:
	nix-shell --pure --command "cabal build"

test:
	nix-shell --pure --command "cabal test"

run:
	nix-shell --pure --command "cabal run"
