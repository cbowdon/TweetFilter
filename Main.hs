{-# LANGUAGE OverloadedStrings #-}
-- | Download tweets, store in database and generate Bayesian filter
module Main where

import Control.Monad.Error
import Control.Monad.Reader
import Data.Char
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.Environment
import Tweet.Bayesian
import Tweet.Store
import Tweet.Store.Connection
import Tweet.Types
import Data.Aeson (eitherDecode)

tokenFile :: FilePath
tokenFile = "auth/bear_token.json"

url :: String
url = "https://api.twitter.com/1.1/search/tweets.json?q=code&lang=en"

printUsage :: IO ()
printUsage = mapM_ putStrLn [   "Takes 1 command line arg:",
                                "\ttrain    = download tweets to build up database - must be human classified",
                                "\tclassify = download tweets and attempt to classify whether spam or not"  ]

-- | Download tweets, store in database and generate Bayesian filter
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["train"]       -> train
        ["classify"]    -> classify
        _               -> printUsage

classify :: IO ()
classify = withConnection $ \conn -> do
    eitherProbs conn >>= print
    print "Probabilities"
    where
        eitherProbs c = runErrorT $ do
            token <- readToken tokenFile
            tweets <- download token
            stats <- liftIO $ runReaderT loadStats c
            return [classifyTweet stats t | t <- statuses tweets]

classifyTweet :: Stats -> Tweet -> Prob
classifyTweet stats tweet = spamScore wds stats
    where
        wds = extractWords $ text tweet

train :: IO ()
train = do
    eitherTweets >>= print
    print "Now you must classify the tweets. Manually. Have fun."
    where
        eitherTweets = runErrorT $ do
            token <- readToken tokenFile
            tweets <- download token
            liftIO $ store tweets
            return tweets

humanClassify :: Tweet -> ErrorT String IO Bool
humanClassify tweet = do
    _ <- liftIO $ print tweet
    _ <- liftIO $ putStr "Is it spam? y/n "
    input <- liftIO getLine
    case map toLower input of
        "y" -> return True
        "n" -> return False
        x   -> throwError $ "Input not understood" ++ x

readToken :: FilePath -> ErrorT String IO Token
readToken filePath = ErrorT $ do
    contents <- BL.readFile filePath
    return $ eitherDecode contents

download :: Token -> ErrorT String IO Tweets
download token = ErrorT $ do
    req <- liftIO $ parseUrl url
    let req' = req {
        requestHeaders = [(hAuthorization, BC.pack $ "Bearer " ++ accessToken token)]
    }
    res <- liftIO $ withManager $ httpLbs req'
    return $ eitherDecode $ responseBody res

store :: Tweets -> IO ()
store tweets =
    withConnection $ \conn -> mapM_ (insert' conn) $ statuses tweets
    where
        insert' c t = runReaderT (insert t) c
