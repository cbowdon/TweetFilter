module Store.Test where

import Store
import Store.Connection
import TwitterTypes
import Control.Monad
import Database.HDBC (IConnection)
import Test.QuickCheck
import Test.QuickCheck.Monadic

randomString :: Gen String
randomString = listOf1 randomChar
    where
        randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

prop_parseTest :: (Eq a, FromSQL a, ToSQL a) => a -> Bool
prop_parseTest a = Just a == (parseSQL . prepSQL $ a)

insertTest :: IConnection c => (c -> a -> IO Integer) -> c -> a -> IO Bool
insertTest f c a = liftM (==1) $ f c a

-- TODO try and reduce this explosion of parameters
selectTest :: (IConnection c, Eq a) => (c -> a -> IO Integer) -> (c -> a -> IO (Maybe [a])) -> c -> a -> IO Bool
selectTest f g c a = do
    _ <- f c a
    m <- g c a
    case m of
        Just a' -> return $ all (a==) a'
        _       -> return False

prop :: IConnection c => (c -> a -> IO Bool) -> c -> a -> Property
prop f c t = monadicIO $ do
    result <- run $ f c t
    assert result

instance Arbitrary Token where
    arbitrary = do
        at <- randomString
        tt <- randomString
        return $ Token at tt

insertToken :: IConnection c => c -> Token -> IO Integer
insertToken c = persist c (SQLExpr "insert into Token (access_token, token_type) values (?, ?)" [])

selectToken :: IConnection c => c -> Token -> IO (Maybe [Token])
selectToken c t = retrieve c (SQLExpr "select * from Token where access_token = ? and token_type = ?" (prepSQL t))

testToken :: IO ()
testToken = withConnection $ \conn -> do
    quickCheck (prop_parseTest :: (Token -> Bool))
    quickCheck $ prop (insertTest insertToken) conn
    quickCheck $ prop (selectTest insertToken selectToken) conn

instance Arbitrary User where
    arbitrary = do
        u <- randomString
        n <- randomString
        sn <- randomString
        return $ User u n sn

insertUser :: IConnection c => c -> User -> IO Integer
insertUser c = persist c (SQLExpr "insert into User (id, name, screen_name) values (?, ?, ?)" [])

selectUser :: IConnection c => c -> User -> IO (Maybe [User])
selectUser c u = retrieve c (SQLExpr "select * from User where id = ? and name = ? and screen_name = ?" (prepSQL u))

prop_parseUser :: User -> Bool
prop_parseUser u = Just u == (parseSQL . prepSQL $ u)

testUser :: IO ()
testUser = withConnection $ \conn -> do
        quickCheck (prop_parseTest :: (User -> Bool))
        quickCheck $ prop (insertTest insertUser) conn
        quickCheck $ prop (selectTest insertUser selectUser) conn
