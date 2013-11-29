module Bayesian.Test
( testWordFreq
, testRelativeFreq
, testSpamProb
, testCombinedProbs
) where

import qualified Data.Map as Map
import Test.QuickCheck
import Bayesian
import TwitterTypes

between :: Double -> Double -> Double -> Bool
between a b x = a <= x && x <= b

prop_sumWords :: Words -> Bool
prop_sumWords w = sumValues == (length $ runWords w)
    where
        sumValues = Map.foldr (+) 0 . runDict . wordFreqs $ w

prop_distinctWords :: Words -> Bool
prop_distinctWords w = distinctWords <= (length $ runWords w)
    where
        distinctWords = Map.size . runDict . wordFreqs $ w

prop_fromTweet :: Tweet -> Bool
prop_fromTweet t = prop_sumWords w && prop_distinctWords w
    where
         w = extractWords $ text t

testWordFreq :: IO ()
testWordFreq = do
    quickCheck prop_sumWords
    quickCheck prop_distinctWords
    quickCheck prop_fromTweet

prop_sumFreqs :: Dict -> Bool
prop_sumFreqs d = (1.0 - sumKeys) < 1e-6
    where
        keys'           = Map.keys . runDict
        relativeFreq' w = relativeFreq w d
        sumKeys         = abs $ sum . map relativeFreq' . keys' $ d

prop_rfZeroOne :: Dict -> Bool
prop_rfZeroOne d = all betweenZeroOne  . map relativeFreq' . keys' $ d
    where
        keys'               = Map.keys . runDict
        betweenZeroOne x    = x > 0 && x <= 1
        relativeFreq' w     = relativeFreq w d

testRelativeFreq :: IO ()
testRelativeFreq = do
    quickCheck prop_sumFreqs
    quickCheck prop_rfZeroOne

prop_98pc :: Dict -> Dict -> Bool
prop_98pc goodCounts badCounts = all (between 0.01 0.99) spamProbs
    where
        badWords    = Map.keys . runDict $ badCounts
        spamProbs   = map (\w -> runProb $ spamProb w goodCounts badCounts) $ badWords

testSpamProb :: IO ()
testSpamProb = quickCheck prop_98pc

prop_cpZeroOne :: [Prob] -> Bool
prop_cpZeroOne = between 0 1 . runProb . combinedProb

testCombinedProbs :: IO ()
testCombinedProbs = quickCheck prop_cpZeroOne
