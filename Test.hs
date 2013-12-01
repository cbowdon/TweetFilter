module Main where

import Test.HUnit
import Tweet.Test.Bayesian
import Tweet.Test.Store

main :: IO ()
main = do
    print "Spam score"
    _ <- runTestTT testSpamScore
    print "Most interesting"
    testMostInteresting
    print "Combined probabilities"
    testCombinedProbs
    print "Spam probability"
    testSpamProb
    print "Word frequency"
    testWordFreq
    print "Relative frequency"
    testRelativeFreq
    print "Token"
    testToken
    print "User"
    testUser
    print "Tweet"
    testTweet
    print "Marking spam"
    testMark
