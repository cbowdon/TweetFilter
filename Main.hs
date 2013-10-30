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
    eToken <- liftM eitherDecode $ BL.readFile "/home/chris/Tweet/auth/bear_token.json" :: IO (Either String Token)
    case eToken of
        Left err    -> print err
        Right token -> do
            req <- parseUrl "https://api.twitter.com/1.1/search/tweets.json?q=code&lang=en"
            let req' = req {
                requestHeaders = [(hAuthorization, BC.pack $ "Bearer " ++ accessToken token)]
            }
            res <- withManager $ httpLbs req'
            print res
            let body = eitherDecode $ responseBody res :: Either String Tweets
            case body of
                Left err        -> print err
                Right tweets    -> withConnection $ \conn -> do
                    mapM_ (insert conn) $ statuses tweets
                    print tweets
