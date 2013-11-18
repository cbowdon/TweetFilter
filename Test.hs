module Main where

import Store.Test

main :: IO ()
main = do
    print "Token"
    testToken
    print "User"
    testUser
    print "Tweet"
    testTweet
    print "Marking spam"
    testMark
