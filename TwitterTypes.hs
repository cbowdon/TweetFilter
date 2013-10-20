{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TwitterTypes
( Token(..)
, User(..)
, Tweet(..)
, Tweets(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import GHC.Generics

data Token = Token  { accessToken :: String
                    , tokenType :: String
                    } deriving (Show, Generic)

data User = User    { id :: String
                    , name :: String
                    , screenName :: String
                    } deriving (Show, Generic)

data Tweet = Tweet  { text :: String
                    , user:: User
                    } deriving (Show, Generic)

data Tweets = Tweets    { statuses :: [Tweet]
                        } deriving (Show, Generic)

instance FromJSON Token where
    parseJSON (Object v) =
        Token   <$> (v .: "access_token")
                <*> (v .: "token_type")
    parseJSON _         = mzero

instance FromJSON User where
    parseJSON (Object v) =
        User    <$> (v .: "id_str")
                <*> (v .: "name")
                <*> (v .: "screen_name")
    parseJSON _         = mzero

instance FromJSON Tweet
instance FromJSON Tweets
