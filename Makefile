build:
	cabal build

setup:
	cabal sandbox init
	cabal install --only-dependencies
	cabal configure
