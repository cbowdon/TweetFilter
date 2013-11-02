{-# LANGUAGE OverloadedStrings #-}
-- | Download tweets, store in database and generate Bayesian filter
module Main where

import Control.Monad
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import TwitterTypes
import Store
import Store.Connection
import Store.Test
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

-- | Download tweets, store in database and generate Bayesian filter
main :: IO ()
main = do
    eToken <- readToken
    eTweets <- download eToken
    store eTweets

readToken :: IO (Either String Token)
readToken = liftM eitherDecode $ BL.readFile "/home/chris/Tweet/auth/bear_token.json"

download :: Either String Token -> IO (Either String Tweets)
download eToken =
    case eToken of
        Left err    -> return $ Left err
        Right token -> do
            req <- parseUrl "https://api.twitter.com/1.1/search/tweets.json?q=code&lang=en"
            let req' = req {
                requestHeaders = [(hAuthorization, BC.pack $ "Bearer " ++ accessToken token)]
            }
            res <- withManager $ httpLbs req'
            print res
            let body = eitherDecode $ responseBody res :: Either String Tweets
            return body

store :: Either String Tweets -> IO ()
store eTweets =
    case eTweets of
        Left err        -> print err
        Right tweets    -> withConnection $ \conn -> do
            mapM_ (insert conn) $ statuses tweets
            print tweets
