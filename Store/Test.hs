module Store.Test where

import Store
import Store.Connection
import TwitterTypes
import Control.Monad
import Database.HDBC (IConnection)
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_parseTest :: (Eq a, FromSQL a, ToSQL a) => a -> Bool
prop_parseTest a = Just a == (parseSQL . prepSQL $ a)

insertTest :: (ToSQL a, IConnection c) => c -> a -> IO Bool
insertTest c a = liftM (==1) $ insert c a

selectTest :: (IConnection c, Eq a, FromSQL a, ToSQL a) => c -> a -> IO Bool
selectTest c a = do
    _ <- insert c a
    m <- select c a
    case m of
        Just a' -> return $ all (a==) a'
        _       -> return False

prop :: IConnection c => (c -> a -> IO Bool) -> c -> a -> Property
prop f c t = monadicIO $ do
    result <- run $ f c t
    assert result

testToken :: IO ()
testToken = withConnection $ \conn -> do
    quickCheck (prop_parseTest :: Token -> Bool)
    quickCheck (prop insertTest conn :: Token -> Property)
    quickCheck (prop selectTest conn :: Token -> Property)

testUser :: IO ()
testUser = withConnection $ \conn -> do
        quickCheck (prop_parseTest :: User -> Bool)
        quickCheck (prop insertTest conn :: User -> Property)
        quickCheck (prop selectTest conn :: User -> Property)
