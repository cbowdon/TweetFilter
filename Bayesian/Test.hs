module Bayesian.Test
( testWordFreq
, testRelativeFreq
) where

import qualified Data.Map as Map
import Test.QuickCheck
import Bayesian
import TwitterTypes

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

prop_zeroOne :: Dict -> Bool
prop_zeroOne d = all betweenZeroOne  . map relativeFreq' . keys' $ d
    where
        keys'               = Map.keys . runDict
        betweenZeroOne x    = x > 0 && x <= 1
        relativeFreq' w     = relativeFreq w d

testRelativeFreq :: IO ()
testRelativeFreq = do
    quickCheck prop_sumFreqs
    quickCheck prop_zeroOne
