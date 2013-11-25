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

-- TODO not quite right
prop_sumFreqs :: Dict -> Bool
prop_sumFreqs d = (sum . map (\w -> relativeFreq w d) . Map.keys . runDict $ d) == 1

prop_zeroOne :: Dict -> Bool
prop_zeroOne d = all (\x -> x > 0 && x <= 1) . map (\w -> relativeFreq w d) . Map.keys . runDict $ d

testRelativeFreq :: IO ()
testRelativeFreq = do
    quickCheck prop_sumFreqs
    quickCheck prop_zeroOne


