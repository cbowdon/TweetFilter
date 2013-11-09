{-# LANGUAGE OverloadedStrings #-}
-- | Download tweets, store in database and generate Bayesian filter
module Main where

import Control.Monad.Error
import Control.Monad.Reader
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import TwitterTypes
import Store
import Store.Connection
import Store.Test
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

tokenFile :: FilePath
tokenFile = "/home/chris/Tweet/auth/bear_token.json"

url :: String
url = "https://api.twitter.com/1.1/search/tweets.json?q=code&lang=en"

-- | Download tweets, store in database and generate Bayesian filter
main :: IO ()
main = eitherTweets >>= print
    where
        eitherTweets = runErrorT $ do
            token <- readToken tokenFile
            tweets <- download token
            liftIO $ store tweets
            return tweets

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
    liftIO $ print res
    return $ eitherDecode $ responseBody res

store :: Tweets -> IO ()
store tweets =
    withConnection $ \conn -> do
        mapM_ (\t -> runReaderT (insert t) conn) $ statuses tweets
        print tweets
