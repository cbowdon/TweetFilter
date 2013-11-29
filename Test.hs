module Main where

import Bayesian.Test
import Store.Test

main :: IO ()
main = do
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
