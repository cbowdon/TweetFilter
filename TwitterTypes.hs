{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TwitterTypes where

import Data.Aeson (FromJSON(..))
import GHC.Generics

data Token = Token  { access_token :: String
                    , token_type :: String
                    } deriving (Show, Generic)

data User = User    { id :: Integer
                    , name :: String
                    , screen_name :: String
                    } deriving (Show, Generic)

data Tweet = Tweet  { text :: String
                    , user:: User
                    } deriving (Show, Generic)

data Tweets = Tweets    { statuses :: [Tweet]
                        } deriving (Show, Generic)

instance FromJSON Token
instance FromJSON User
instance FromJSON Tweet
instance FromJSON Tweets

