MAIN=Main.hs
DB="tweets.db"
DIST="dist"

all:
	runhaskell $(MAIN)

compile:
	cabal install \
		--haddock-executables \
		--disable-documentation \
		--ghc-option=-Wall
	make lint

run:
	cabal run

lint:
	hlint *.hs Store/*.hs

clean:
	rm -f *.o *.hi Store/*.o Store/*.hi
	rm -rf $(DIST)

db:
	sqlite3 $(DB) < scripts/create_db.sql

test: lint
	ghc -Wall Test.hs -o RunTest
	./RunTest
	rm RunTest
	make clean
