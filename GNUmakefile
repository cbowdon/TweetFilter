MAIN=Main.hs
DB="tweets.db"
DIST="dist"

all:
	runhaskell $(MAIN)
	make clean

compile:
	cabal install \
		--haddock-executables \
		--disable-documentation \
		--ghc-option=-Wall
	make lint
	make doc

run:
	cabal run

lint:
	hlint *.hs Store/*.hs

doc:
	cabal haddock --executables

clean:
	rm -f *.o *.hi Store/*.o Store/*.hi Store/Raw/*.o Store/Raw/*.hi
	rm -rf $(DIST)

db:
	sqlite3 $(DB) < scripts/create_db.sql

test: lint
	ghc -Wall Test.hs -o RunTest
	./RunTest
	rm RunTest
	make clean
