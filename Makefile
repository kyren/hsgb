build: setup
	cabal build

test: setup
	cabal test

clean:
	cabal clean
	cabal sandbox delete

setup:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests
