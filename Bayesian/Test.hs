module Bayesian.Test where

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
