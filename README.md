# Bayesian Filtering for Twitter Spam

## Tasks
+ Get Twitter auth tokens (bash script)
+ Use tokens to retrieve a bunch of tweets (Haskell)
+ Store tweets somewhere (sqlite3)
+ Train the filter

## Learnings so far

### Bash
- The handy _base64_ program exists - remember to adjust the default column wrap though.
- _echo_ can and will mess up your strings. Use _printf_ if it counts.
- Remember to double-quote variables if the program expects a string.
- Someone out there on the Internet is lying when they say that the latest version of _screen_ allows vertical splits. The latest version is 4.0.3 (which prints its own version number as 4.0.0.3 for some reason) and does not allow vertical splits. There are patched versions with higher numbers that do allow it.

### Haskell on Arch
- Install _ghc_ from pacman.
- Install everything else from _cabal_. It should be included with GHC, you may have to go hunting for the file.

### Haskell
- You can do HTTPS with the _http-conduit_ package.
- Aeson sounds like it will be the least-painful JSON handling.
