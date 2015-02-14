build:
	cabal build

prepare:
	cabal sandbox init
	cabal install --only-dependencies
