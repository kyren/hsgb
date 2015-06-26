build:
	nix-shell --pure --command "cabal build"

test:
	nix-shell --pure --command "cabal test"

run:
	nix-shell --pure --command "cabal run"

clean:
	nix-shell --pure --command "cabal clean"

configure:
	nix-shell --pure --command "cabal configure"
