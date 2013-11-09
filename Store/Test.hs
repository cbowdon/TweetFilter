-- | Tests for the persistence layer
module Store.Test
( testToken
, testUser
, testTweet
) where

import Store
import Store.Connection
import TwitterTypes
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Database.HDBC (IConnection)
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_parseTest :: (Eq a, FromSQL a, ToSQL a) => a -> Bool
prop_parseTest a = Just a == (parseSQL . prepSQL $ a)

insertTest :: (ToSQL a, IConnection c) => a -> ReaderT c IO Bool
insertTest a = liftM (==1) $ insert a

-- TODO fix for transformers
{-
selectTest :: (IConnection c, Eq a, FromSQL a, ToSQL a) => c -> a -> MaybeT IO Bool
selectTest c a = do
    _ <- runReaderT (insert a) c
    m <- select c a
    case m of
        Just a' -> return $ all (a==) a'
        _       -> return False
-}

prop :: IConnection c => (a -> ReaderT c IO Bool) -> c -> a -> Property
prop f c t = monadicIO $ do
    result <- run $ runReaderT (f t) c
    assert result

-- | Check saving and loading tokens
testToken :: IO ()
testToken = withConnection $ \conn -> do
    quickCheck (prop_parseTest :: Token -> Bool)
    quickCheck (prop insertTest conn :: Token -> Property)
    -- quickCheck (prop selectTest conn :: Token -> Property)

-- | Check saving and loading users
testUser :: IO ()
testUser = withConnection $ \conn -> do
    quickCheck (prop_parseTest :: User -> Bool)
    quickCheck (prop insertTest conn :: User -> Property)
    -- quickCheck (prop selectTest conn :: User -> Property)

-- | Check saving and loading tweets
testTweet :: IO ()
testTweet = withConnection  $ \conn -> do
    quickCheck (prop insertTest conn :: Tweet -> Property)
    -- quickCheck (prop selectTest conn :: Tweet -> Property)
