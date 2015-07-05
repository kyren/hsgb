export PATH := $(HOME)/.nix-profile/bin:$(PATH)
export NIX_PATH := nixpkgs=$(HOME)/.nix-defexpr/channels/nixpkgs

build:
	nix-shell --pure --command "cd ../; cabal build"

test:
	nix-shell --pure --command "cd ../; cabal test"

run:
	nix-shell --pure --command "cd ../; cabal run"

repl:
	nix-shell --pure --command "cd ../; cabal repl"

clean:
	nix-shell --pure --command "cd ../; cabal clean"

configure:
	nix-shell --pure --command "cd ../; cabal configure"
