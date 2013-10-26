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
	rm -f *.o *.hi
	rm -rf $(DIST)

db:
	sqlite3 $(DB) < scripts/create_db.sql
