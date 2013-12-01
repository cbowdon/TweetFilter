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
import Tweet.TwitterTypes
import Tweet.Store
import Tweet.Store.Connection
import Data.Aeson (eitherDecode)

tokenFile :: FilePath
tokenFile = "auth/bear_token.json"

url :: String
url = "https://api.twitter.com/1.1/search/tweets.json?q=code&lang=en"

-- | Download tweets, store in database and generate Bayesian filter
main :: IO ()
main = eitherTweets >>= print
    where
        eitherTweets = runErrorT $ do
            token <- readToken tokenFile
            tweets <- download token
            classes <- mapM humanClassify $ take 5 $ statuses tweets
            liftIO $ print classes
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
