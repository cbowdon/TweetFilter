module Main where

import Bayesian.Test
import Store.Test

main :: IO ()
main = do
    print "Combined Probabilities"
    testCombinedProbs
    print "Spam Probability"
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
