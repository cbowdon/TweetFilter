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
import Database.HDBC
import GHC.Generics
import Store

data Token = Token  { accessToken :: String
                    , tokenType :: String
                    } deriving (Eq, Show, Generic)

data User = User    { uid :: String
                    , name :: String
                    , screenName :: String
                    } deriving (Eq, Show, Generic)

data Tweet = Tweet  { text :: String
                    , user:: User
                    } deriving (Eq, Show, Generic)

data Tweets = Tweets    { statuses :: [Tweet]
                        } deriving (Eq, Show, Generic)

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

instance ToSQL Token where
    prepSQL (Token at tt)   = [toSql at, toSql tt]

instance FromSQL Token where
    parseSQL [at, tt]   = Just $ Token (fromSql at) (fromSql tt)
    parseSQL _          = Nothing

instance ToSQL User where
    prepSQL (User i n sn) = [toSql i, toSql n, toSql sn]

instance FromSQL User where
    parseSQL [i, n, sn] = Just $ User (fromSql i) (fromSql n) (fromSql sn)
    parseSQL _          = Nothing
