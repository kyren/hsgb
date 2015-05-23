build: setup
	cabal build

setup:
	cabal sandbox init
	cabal install --only-dependencies
	cabal configure

clean:
	cabal sandbox delete
