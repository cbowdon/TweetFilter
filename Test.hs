module Main where

import Bayesian.Test
import Store.Test

main :: IO ()
main = do
    print "Word frequency"
    testWordFreq
    print "Token"
    testToken
    print "User"
    testUser
    print "Tweet"
    testTweet
    print "Marking spam"
    testMark
