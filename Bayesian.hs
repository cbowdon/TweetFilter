-- | Bayesian filtering functionality - build frequency maps and calculating probability
module Bayesian
( -- * Types
Dict(..)
, Words(..)
-- * Functions
, extractWords
, wordFreqs
, merge
, relativeFreq
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

-- | Pair of word and count
data CountedWord = CountedWord { runCountedWord :: (String, Int) } deriving (Eq, Show)

-- | Instance for QuickCheck
instance Arbitrary CountedWord where
    arbitrary = do
        w <- randomString
        f <- choose (1, 140)
        return $ CountedWord (w, f)

-- | Dictionary of words to counts
newtype Dict = Dict { runDict :: Map.Map String Int } deriving (Eq, Show)

-- | Instance for QuickCheck
instance Arbitrary Dict where
    arbitrary = do
        countedWords <- listOf1 $ liftM runCountedWord arbitrary
        return . Dict . Map.fromList $ countedWords

-- TODO we don't need CountedWord

-- | Wrapper for list of strings
newtype Words = Words { runWords :: [String] } deriving (Eq, Show)

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
relativeFreq :: String -> Dict -> Double
relativeFreq w (Dict d) = x / sumX
    where
        sumX    = fromIntegral $ Map.foldr (+) 0 d
        x       = fromIntegral $ fromMaybe 0 (Map.lookup w d)
