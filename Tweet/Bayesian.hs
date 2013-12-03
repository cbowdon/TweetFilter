-- | Bayesian filtering functionality - build frequency maps and calculating probability
module Tweet.Bayesian
( -- * Types
Stats(..)
, Dict(..)
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
, mostInteresting
, spamScore
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
newtype Word = Word { runWord :: String } deriving (Eq, Ord, Read)

-- | Pretty print
instance Show Word where
    show (Word w) = show w

-- | Instance for QuickCheck
instance Arbitrary Word where
    arbitrary = do
        s <- randomString
        return $ Word s

-- | A probability
newtype Prob = Prob { runProb :: Double } deriving (Eq, Ord)

-- | Instance for QuickCheck
instance Arbitrary Prob where
    arbitrary = do
        p <- choose (0.0, 1.0)
        return . Prob $ p

-- | Pretty print
instance Show Prob where
    show (Prob p) = 'p' : show p

-- | Dictionary of words to counts
newtype Dict = Dict { runDict :: Map.Map Word Int } deriving (Eq, Show)

-- | Instance for QuickCheck
instance Arbitrary Dict where
    arbitrary = do
        ws <- listOf1 arbitrary
        is <- listOf1 $ choose (1, 140)
        return . Dict . Map.fromList $ zip ws is

-- | Statistics of good/bad words
data Stats = Stats  { goodCounts :: Dict, badCounts :: Dict } deriving (Eq, Show)

instance Arbitrary Stats where
    arbitrary = liftM2 Stats arbitrary arbitrary

extractWords :: String -> [Word]
extractWords w = [Word $ alphabetical w' | w' <- words w, nonTrivial w']
    where
        nonTrivial = (> 2) . length
        alphabetical s =
            case head s of
                '@' -> s
                _   -> filter (\x -> elem x $ ['A'..'Z'] ++ ['a'..'z']) s

-- | Create dictionary of word frequencies from a list of words
wordFreqs :: [Word] -> Dict
wordFreqs = Dict . Map.fromList . map (head &&& length) . List.group . List.sort

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
spamProb :: Word -> Stats-> Prob
spamProb word (Stats gd bd)
    | p < 0.01  = Prob 0.01
    | p > 0.99  = Prob 0.99
    | otherwise = Prob p
    where
        rfGood  = relativeFreq word gd
        rfBad   = relativeFreq word bd
        p       = if rfBad == 0 then 0 else rfBad / (rfGood + rfBad)

-- | A blunt measure of significance (deviation from mean)
interestingness :: Prob -> Double
interestingness (Prob p) = abs $ p - 0.5

-- Combined probability that a message is spam
combinedProb :: [Prob] -> Prob
combinedProb p
    | num == 0  = Prob 0
    | otherwise = Prob $ num / denom
    where
        -- TODO this would benefit from stream fusion
        p'  = [runProb x | x <- p]
        mp' = [1 - runProb x | x <- p]
        num = product p'
        denom = product p' + product mp'

-- | Calculates the n most interesting words
mostInteresting :: Int -> [Word] -> Stats -> [(Word, Prob)]
mostInteresting n wds stats = take n . List.sortBy (flip comp) $ f
    where
        f = [(w', spamProb w' stats) | w' <-  wds]
        comp (_,p) (_,p') = compare (interestingness p) (interestingness p')

spamScore :: [Word] -> Stats -> Prob
spamScore wds stats = combinedProb [p | (_,p) <- mostInteresting 5 wds stats]
