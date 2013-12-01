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
	make clean

run:
	cabal run

lint:
	find . -name "*.hs" | xargs hlint

doc:
	cabal haddock --executables

clean:
	find . -name "*.hi" | xargs rm
	find . -name "*.o" | xargs rm
	rm -rf $(DIST)

db:
	sqlite3 $(DB) < scripts/create_db.sql

test: compile-tests db
	./RunTest
	rm RunTest

compile-tests:
	ghc -Wall Test.hs -o RunTest
	make lint
	make clean
