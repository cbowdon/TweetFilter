{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | Types associated with the Twitter Search API
module Tweet.TwitterTypes
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
import Tweet.Spam
import Tweet.Store
import qualified Tweet.Store.Raw.Insert as Insert
import qualified Tweet.Store.Raw.Select as Select
import qualified Tweet.Store.Raw.Update as Update

randomString :: Gen String
randomString = listOf1 randomChar
    where
        randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ " .,_-;:!<>\\[]+=@#$%^&&"

-- | An authentication token from Twitter
data Token = Token  { accessToken :: String
                    , tokenType :: String
                    } deriving (Eq, Show, Generic)

instance FromJSON Token where
    parseJSON (Object v) =
        Token   <$> (v .: "access_token")
                <*> (v .: "token_type")
    parseJSON _         = mzero

instance IdSQL Token where
    idSQL (Token at tt) = [toSql at, toSql tt]

instance ToSQL Token where
    prepSQL (Token at tt) = [toSql at, toSql tt]
    insert = persist $ SQLExpr Insert.token []

instance FromSQL Token where
    parseSQL [at, tt]   = Just $ Token (fromSql at) (fromSql tt)
    parseSQL _          = Nothing
    select = retrieve . SQLExpr Select.token . idSQL

instance Arbitrary Token where
    arbitrary = do
        at <- randomString
        tt <- randomString
        return $ Token at tt

-- | A Twitter user
data User = User    { uid :: String
                    , name :: String
                    , screenName :: String
                    , spammer :: Maybe Bool
                    } deriving (Eq, Show, Generic)


instance FromJSON User where
    parseJSON (Object v) =
        User    <$> (v .: "id_str")
                <*> (v .: "name")
                <*> (v .: "screen_name")
                <*> (v .:? "spammer")
    parseJSON _         = mzero

instance IdSQL User where
    idSQL (User i _ _ _) = [toSql i]

instance ToSQL User where
    prepSQL (User i n sn sp) =
        case sp of
            Nothing -> [toSql i, toSql n, toSql sn, SqlNull]
            Just v  -> [toSql i, toSql n, toSql sn, toSql v]
    insert = persist $ SQLExpr Insert.user []

instance FromSQL User where
    parseSQL [i, n, sn, sp] = Just $ User (fromSql i) (fromSql n) (fromSql sn) (fromSql sp)
    parseSQL _          = Nothing
    select = retrieve . SQLExpr Select.user . idSQL

instance Arbitrary User where
    arbitrary = do
        u <- randomString
        n <- randomString
        sn <- randomString
        sp <- arbitrary :: Gen (Maybe Bool)
        return $ User u n sn sp

instance Spam User where
    mark b = modify $ SQLExpr Update.markUser [toSql b]
    isSpam = spammer

-- | Core information in a Tweet: the text content and the user
data Tweet = Tweet  { text :: String
                    , user :: User
                    , spam :: Maybe Bool
                    } deriving (Eq, Show, Generic)

instance FromJSON Tweet

instance IdSQL Tweet where
    idSQL (Tweet t u _) = [toSql t, toSql $ uid u]

instance ToSQL Tweet where
    prepSQL (Tweet t u s) =
        case s of
            Nothing ->  [toSql t, toSql $ uid u, SqlNull]
            Just v  ->  [toSql t, toSql $ uid u, toSql v]
    insert tweet = do
        _ <- insert $ user tweet
        persist (SQLExpr Insert.tweet []) tweet

instance FromSQL Tweet where
    parseSQL (t:s:u)  = do
        u' <- parseSQL u
        return $ Tweet (fromSql t) u' (Just . fromSql $ s)
    parseSQL _      = Nothing
    select = retrieve . SQLExpr Select.tweet . idSQL

instance Arbitrary Tweet where
    arbitrary = do
        t <- randomString
        u <- arbitrary :: Gen User
        s <- arbitrary :: Gen (Maybe Bool)
        return $ Tweet t u s

instance Spam Tweet where
    mark b = modify $ SQLExpr Update.markTweet [toSql b]
    isSpam = spam

-- | A collection of Tweets (provided for JSON compatibility)
data Tweets = Tweets    { statuses :: [Tweet]
                        } deriving (Eq, Show, Generic)

instance FromJSON Tweets

instance Arbitrary Tweets where
    arbitrary = do
        s <- listOf arbitrary :: Gen [Tweet]
        return $ Tweets s
