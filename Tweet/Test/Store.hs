-- | Tests for the persistence layer
module Tweet.Test.Store
( testToken
, testUser
, testTweet
, testMark
) where

{-
 - Warning to future generations:
 - testing DB functionality with QuickCheck was not necessarily a good idea.
 - Should add some HUnit tests here.
 -}

import Control.Monad
import Control.Monad.Reader
import Database.HDBC (IConnection)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tweet.Spam
import Tweet.Store
import Tweet.Store.Connection
import Tweet.Types

prop_parseTest :: (Eq a, FromSQL a, ToSQL a) => a -> Bool
prop_parseTest a = Just a == (parseSQL . prepSQL $ a)

insertTest :: (ToSQL a, IConnection c) => a -> ReaderT c IO Bool
insertTest a = liftM (==1) $ insert a

selectTest :: (IConnection c, Eq a, FromSQL a, ToSQL a) => a -> ReaderT c IO Bool
selectTest a = do
    _ <- insert a
    a' <- select a
    return $ all (a==) a'

markTest :: (IConnection c, FromSQL a, ToSQL a, Spam a, Show a) => a -> ReaderT c IO Bool
markTest a = do
    _ <- insert a
    _ <- mark True a
    a' <- select a
    case a' of
        [a'']   -> return $ isSpam a'' == Just True
        _       -> return False

prop :: IConnection c => (a -> ReaderT c IO Bool) -> c -> a -> Property
prop f c t = monadicIO $ do
    result <- run $ runReaderT (f t) c
    assert result

-- | Check saving and loading tokens
testToken :: IO ()
testToken = withConnection $ \conn -> do
    quickCheck (prop_parseTest :: Token -> Bool)
    quickCheck (prop insertTest conn :: Token -> Property)
    quickCheck (prop selectTest conn :: Token -> Property)

-- | Check saving and loading users
testUser :: IO ()
testUser = withConnection $ \conn -> do
    quickCheck (prop_parseTest :: User -> Bool)
    quickCheck (prop insertTest conn :: User -> Property)
    quickCheck (prop selectTest conn :: User -> Property)

-- | Check saving and loading tweets
testTweet :: IO ()
testTweet = withConnection  $ \conn -> do
    quickCheck (prop insertTest conn :: Tweet -> Property)
    quickCheck (prop selectTest conn :: Tweet -> Property)

-- | Check marking tweets and users as spam
testMark :: IO ()
testMark = withConnection $ \conn -> do
    quickCheck (prop markTest conn :: User -> Property)
    quickCheck (prop markTest conn :: Tweet -> Property)
