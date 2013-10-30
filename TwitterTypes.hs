{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | Types associated with the Twitter Search API
module TwitterTypes
( -- * Types
Token(..)
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
import qualified Store.Raw.Insert as Insert
import qualified Store.Raw.Select as Select

randomString :: Gen String
randomString = listOf1 randomChar
    where
        randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

-- | An authentication token from Twitter
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
    insert c = persist c (SQLExpr Insert.token [])

instance FromSQL Token where
    parseSQL [at, tt]   = Just $ Token (fromSql at) (fromSql tt)
    parseSQL _          = Nothing
    select c t = retrieve c (SQLExpr Select.token (prepSQL t))

instance Arbitrary Token where
    arbitrary = do
        at <- randomString
        tt <- randomString
        return $ Token at tt

-- | A Twitter user
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
    insert c = persist c $ SQLExpr Insert.user []

instance FromSQL User where
    parseSQL [i, n, sn] = Just $ User (fromSql i) (fromSql n) (fromSql sn)
    parseSQL _          = Nothing
    select c u = retrieve c $ SQLExpr Select.user (prepSQL u)

instance Arbitrary User where
    arbitrary = do
        u <- randomString
        n <- randomString
        sn <- randomString
        return $ User u n sn

-- | Core information in a Tweet: the text content and the user
data Tweet = Tweet  { text :: String
                    , user:: User
                    } deriving (Eq, Show, Generic)

instance FromJSON Tweet

instance ToSQL Tweet where
    prepSQL (Tweet t u) = [toSql t, toSql $ uid u]
    insert c tweet = do
        _ <- insert c $ user tweet
        persist c (SQLExpr Insert.tweet []) tweet

instance FromSQL Tweet where
    parseSQL (t:u)  = parseSQL u >>= Just . Tweet (fromSql t)
    parseSQL _      = Nothing
    select c t = retrieve c $ SQLExpr Select.tweet (prepSQL t)

instance Arbitrary Tweet where
    arbitrary = do
        t <- randomString
        u <- arbitrary :: Gen User
        return $ Tweet t u

-- | A collection of Tweets (provided for JSON compatibility)
data Tweets = Tweets    { statuses :: [Tweet]
                        } deriving (Eq, Show, Generic)

instance FromJSON Tweets

instance Arbitrary Tweets where
    arbitrary = do
        s <- listOf arbitrary :: Gen [Tweet]
        return $ Tweets s
