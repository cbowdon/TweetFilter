-- | Bayesian filtering functionality - build frequency maps and calculating probability
module Bayesian
( -- * Types
Dict(..)
, Words(..)
, Prob(..)
, Word(..)
-- * Functions
, extractWords
, wordFreqs
, merge
, relativeFreq
, spamProb
, interestingness
, combinedProb
) where

import Control.Arrow
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import Test.QuickCheck

randomString :: Gen String
randomString = listOf1 randomChar
    where
        randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ",.'\",-"

-- | Type def for single words from a sentence
type Word = String
-- newtype Word = Word { runWord :: String } deriving (Eq, Show)

-- | A probability
newtype Prob = Prob { runProb :: Double } deriving (Eq, Show)

instance Arbitrary Prob where
    arbitrary = do
        p <- choose (0.0, 1.0)
        return . Prob $ p

-- | Pair of word and count
data CountedWord = CountedWord { runCountedWord :: (Word, Int) } deriving (Eq, Show)

-- | Instance for QuickCheck
instance Arbitrary CountedWord where
    arbitrary = do
        w <- randomString
        f <- choose (1, 140)
        return $ CountedWord (w, f)

-- | Dictionary of words to counts
newtype Dict = Dict { runDict :: Map.Map Word Int } deriving (Eq, Show)

-- | Instance for QuickCheck
instance Arbitrary Dict where
    arbitrary = do
        countedWords <- listOf1 $ liftM runCountedWord arbitrary
        return . Dict . Map.fromList $ countedWords

-- TODO we don't need CountedWord

-- | Wrapper for list of strings
-- TODO could be redundant given Word above
newtype Words = Words { runWords :: [Word] } deriving (Eq, Show)

-- | Instance for QuickCheck
instance Arbitrary Words where
    arbitrary = liftM Words $ listOf randomString

-- | Extract non-trivial, alphabetical (or @name) words from a tweet
extractWords :: String -> Words
extractWords = Words . map alphabetical . filter nonTrivial . words
    where
        nonTrivial = (> 2) . length
        alphabetical w =
            case head w of
                '@' -> w
                _   -> filter (\x -> elem x $ ['A'..'Z'] ++ ['a'..'z']) w

-- | Create dictionary of word frequencies from a list of words
wordFreqs :: Words -> Dict
wordFreqs = Dict . Map.fromList . map (head &&& length) . List.group . List.sort . runWords

-- | Merge two dictionaries, summing values
merge :: Dict -> Dict -> Dict
merge d d' = Dict $ Map.unionWith (+) (runDict d) (runDict d')

-- | Get the relative frequency of a word in a dictionary
relativeFreq :: Word -> Dict -> Double
relativeFreq w (Dict d) = x / sumX
    where
        x       = fromIntegral $ fromMaybe 0 (Map.lookup w d)
        sumX    = fromIntegral $ Map.foldr (+) 0 d

-- | Get the probability that a word is spam
spamProb :: Word -> Dict -> Dict-> Prob
spamProb word goodCounts badCounts
    | p < 0.01  = Prob 0.01
    | p > 0.99  = Prob 0.99
    | otherwise = Prob p
    where
        rfGood  = relativeFreq word goodCounts
        rfBad   = relativeFreq word badCounts
        p       = rfBad / (rfGood + rfBad)

interestingness :: Prob -> Double
interestingness (Prob p) = abs $ p - 0.5

-- Combined probability that a message is spam
combinedProb :: [Prob] -> Prob
combinedProb p = Prob $ num / denom
    where
        prod = foldr ((*) . runProb) 1.0
        num = prod p
        denom = prod p + foldr ((\b a -> b * (1 - a)) . runProb) 1.0 p

-- TODO 4 params, the pair of dicts at least is prime candidate for Reader monad
nMostInteresting :: Int -> [Word] -> Dict -> Dict -> [(Word, Prob)]
nMostInteresting n w goodCounts badCounts = take n . List.sortBy comp . map f $ w
    where
        f w = (w, spamProb w goodCounts badCounts)
        comp (_,p) (_,p') = compare (interestingness p) (interestingness p')

