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
import Test.QuickCheck
import Store

randomString :: Gen String
randomString = listOf1 randomChar
    where
        randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']


data Token = Token  { accessToken :: String
                    , tokenType :: String
                    } deriving (Eq, Show, Generic)

instance FromJSON Token where
    parseJSON (Object v) =
        Token   <$> (v .: "access_token")
                <*> (v .: "token_type")
    parseJSON _         = mzero

instance ToSQL Token where
    prepSQL (Token at tt)   = [toSql at, toSql tt]
    insert c = persist c (SQLExpr "insert into Token (access_token, token_type) values (?, ?)" [])

instance FromSQL Token where
    parseSQL [at, tt]   = Just $ Token (fromSql at) (fromSql tt)
    parseSQL _          = Nothing
    select c t = retrieve c (SQLExpr "select * from Token where access_token = ? and token_type = ?" (prepSQL t))

instance Arbitrary Token where
    arbitrary = do
        at <- randomString
        tt <- randomString
        return $ Token at tt

data User = User    { uid :: String
                    , name :: String
                    , screenName :: String
                    } deriving (Eq, Show, Generic)


instance FromJSON User where
    parseJSON (Object v) =
        User    <$> (v .: "id_str")
                <*> (v .: "name")
                <*> (v .: "screen_name")
    parseJSON _         = mzero

instance ToSQL User where
    prepSQL (User i n sn) = [toSql i, toSql n, toSql sn]
    insert c = persist c (SQLExpr "insert into User (id, name, screen_name) values (?, ?, ?)" [])

instance FromSQL User where
    parseSQL [i, n, sn] = Just $ User (fromSql i) (fromSql n) (fromSql sn)
    parseSQL _          = Nothing
    select c u = retrieve c (SQLExpr "select * from User where id = ? and name = ? and screen_name = ?" (prepSQL u))

instance Arbitrary User where
    arbitrary = do
        u <- randomString
        n <- randomString
        sn <- randomString
        return $ User u n sn

data Tweet = Tweet  { text :: String
                    , user:: User
                    } deriving (Eq, Show, Generic)

instance FromJSON Tweet

instance Arbitrary Tweet where
    arbitrary = do
        t <- randomString
        u <- arbitrary :: Gen User
        return $ Tweet t u

data Tweets = Tweets    { statuses :: [Tweet]
                        } deriving (Eq, Show, Generic)

instance FromJSON Tweets

instance Arbitrary Tweets where
    arbitrary = do
        s <- listOf arbitrary :: Gen [Tweet]
        return $ Tweets s
