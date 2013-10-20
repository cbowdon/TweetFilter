# Bayesian Filtering for Twitter Spam

## Tasks
+ Get Twitter auth tokens (bash script)
+ Use tokens to retrieve a bunch of tweets (Haskell)
+ Store tweets somewhere (sqlite3)
+ Train the filter

## Learnings so far

### Bash
- The handy _base64_ program exists - remember to adjust the default column wrap though.
- _echo_ can and will mess up your strings. Use _printf_ if it counts, or maybe _echo -n_.
- Remember to double-quote variables if the program expects a string.
- Someone out there on the Internet is lying when they say that the latest version of _screen_ allows vertical splits. The latest version is 4.0.3 (which prints its own version number as 4.0.0.3 for some reason) and does not allow vertical splits. There are patched versions with higher numbers that do allow it.
- _screen -e^Bb_ (or _escape ^Bb_ in _.screenrc_) to save your sanity.

### VirtualBox
- Don't trust NAT adapters, use bridged for serious work.

### Haskell on Arch
- Install _ghc_ from pacman.
- Install everything else from _cabal_. It should be included with GHC, you may have to go hunting for the file.
- _hlint_ would not install.

### Haskell
- You can do HTTPS with the _http-conduit_ package.
- Aeson seems to be the least-painful JSON handling library available.

## Aeson
- If you can tolerate using the JSON key names in your data type, using the _DeriveGenerics_ extension takes away a lot of boilerplate by creating your instances for you.
- Use _eitherDecode_ rather than _decode_ to get error messages when it fails.
