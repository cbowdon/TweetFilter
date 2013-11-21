-- | Bayesian filtering functionality - build frequency maps and calculating probability
module Bayesian
( -- * Types
Dict(..)
, Words(..)
-- * Functions
, extractWords
, wordFreqs
, merge
) where

import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import Test.QuickCheck

randomString :: Gen String
randomString = listOf1 randomChar
    where
        randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ",.'\",-"

-- | Dictionary of words to counts
newtype Dict = Dict { runDict :: Map.Map String Int } deriving (Eq, Show)

newtype Words = Words { runWords :: [String] } deriving (Eq, Show)

instance Arbitrary Words where
    arbitrary = liftM Words $ listOf randomString

extractWords :: String -> Words
extractWords = Words . map alphabetical . filter nonTrivial . words
    where
        nonTrivial = (> 2) . length
        alphabetical = filter (\x -> elem x $ ['A'..'Z'] ++ ['a'..'z'])

wordFreqs :: Words -> Dict
wordFreqs = Dict . Map.fromList . map (head &&& length) . List.group . List.sort . runWords

merge :: Dict -> Dict -> Dict
merge d d' = Dict $ Map.unionWith (+) (runDict d) (runDict d')
