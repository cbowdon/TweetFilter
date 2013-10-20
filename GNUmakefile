MAIN=Main.hs
DIST="dist"

all:
	runhaskell $(MAIN)

compile:
	cabal install \
		--haddock-executables \
		--disable-documentation \
		--ghc-option=-Wall

clean:
	rm -f *.o *.hi
	rm -rf $(DIST)
