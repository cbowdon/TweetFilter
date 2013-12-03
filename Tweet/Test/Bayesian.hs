module Tweet.Test.Bayesian
( testWordFreq
, testRelativeFreq
, testSpamProb
, testCombinedProbs
, testMostInteresting
, testSpamScore
) where

import qualified Data.Map as Map
import qualified Data.List as List
import Test.QuickCheck
import Test.HUnit
import Tweet.Bayesian
import Tweet.Types

between :: Double -> Double -> Double -> Bool
between a b x = a <= x && x <= b

prop_sumWords :: [Word] -> Bool
prop_sumWords w = sumValues == length w
    where
        sumValues = Map.foldr (+) 0 . runDict . wordFreqs $ w

prop_distinctWords :: [Word] -> Bool
prop_distinctWords w = distinctWords <= length w
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

prop_98pc :: Stats -> Bool
prop_98pc stats = all (between 0.01 0.99) spamProbs
    where
        badWords    = Map.keys . runDict . badCounts $ stats
        spamProbs   = map (\w -> runProb $ spamProb w stats) badWords

testSpamProb :: IO ()
testSpamProb = quickCheck prop_98pc

prop_cpZeroOne :: [Prob] -> Bool
prop_cpZeroOne = between 0 1 . runProb . combinedProb

testCombinedProbs :: IO ()
testCombinedProbs = quickCheck prop_cpZeroOne

prop_descOrder :: Stats -> Bool
prop_descOrder stats = List.sort res == reverse res
    where
        res         = map snd $ mostInteresting 5 randWords stats
        randWords   = Map.keys .runDict . badCounts $ stats

testMostInteresting :: IO ()
testMostInteresting = quickCheck prop_descOrder

testSpamScore :: Test
testSpamScore = test [  "Manually calculated case"  ~: combProb     ~=? actualCP,
                        "Manually calc'd most int"  ~: mstInt       ~=? actualMI,
                        "Manually calc'd pSpams"    ~: pSpams       ~=? actualPS,
                        "Manually calc'd gdRFs"     ~: gdRelFreqs   ~=? actualGRF,
                        "Manually calc'd bdRFs"     ~: bdRelFreqs   ~=? actualBRF ]
    where
        rawGd       = [("you",5),("eat",4),("love",8),("hate",8),("sweet",3),("hello",1),("everything",2),("suck",4),("lol",1),("bad",3),("fuck",1)]
        rawBd       = [("free",4),("win",8),("click",9),("here",4),("you",5),("code",4),("sexy",6),("hot",6),("suck",3),("everything",1),("bad",2)]
        gd          = Dict { runDict = Map.fromList [(Word w,i) | (w,i) <- rawGd] }
        bd          = Dict { runDict = Map.fromList [(Word w,i) | (w,i) <- rawBd] }
        stats       = Stats gd bd
        -- these words appear with higher relative frequencies in good than bad
        ws          = map Word ["you", "suck", "bad", "everything", "java"]
{-
 - Human calculations
        gdSum       = 39
        bdSum       = 52
-}
        gdRelFreqs  = [5/40, 4/40, 3/40, 2/40, 0/40] :: [Double]
        bdRelFreqs  = [5/52, 3/52, 2/52, 1/52, 0/52] :: [Double]
        pSpams      = [Prob x | x <- [0.4347826086956522, 0.3658536585365854, 0.33898305084745767, 0.2777777777777778, 0.01]]
        -- as it happens, the interestingness ordering is just reverse order
        mstInt      = reverse $ zip ws pSpams
{-
 - Human calculations
        prod pSpams     = 1.4978042190149247e-4
        prod 1-pSpams   = 0.16940399374516954
-}
        combProb    = Prob 8.83380052359703e-4
        actualCP    = spamScore ws stats
        actualMI    = mostInteresting 5 ws stats
        actualPS    = [spamProb w stats | w <- ws]
        actualGRF   = [relativeFreq w gd | w <- ws]
        actualBRF   = [relativeFreq w bd | w <- ws]
