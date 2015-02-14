build: prepare
	cabal build

repl:
	cabal repl

prepare:
	cabal sandbox init
	cabal install --only-dependencies
