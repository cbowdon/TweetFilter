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

randomChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
randomInt = elements [0..64]

instance Arbitrary Token where
    arbitrary = do
        at <- randomString
        tt <- randomString
        return $ Token at tt

insertToken :: IConnection c => c -> Token -> IO Integer
insertToken c = persist c (SQLExpr "insert into Token (access_token, token_type) values (?, ?)" [])

selectToken :: IConnection c => c -> Token -> IO (Maybe [Token])
selectToken c t = retrieve c (SQLExpr "select * from Token where access_token = ? and token_type = ?" (prepSQL t))

prop :: IConnection c => (c -> a -> IO Bool) -> c -> a -> Property
prop f c t = monadicIO $ do
    result <- run $ f c t
    assert result

prop_insertToken :: IConnection c => c -> Token -> Property
prop_insertToken = prop $ \c t -> liftM (==1) $ insertToken c t

prop_selectToken :: IConnection c => c -> Token -> Property
prop_selectToken = prop $ \c t -> do
    _ <- insertToken c t
    m <- selectToken c t
    case m of
        Just t' -> return $ all (t==) t'
        o       -> return False


prop_parseToken :: Token -> Bool
prop_parseToken t = Just t == (parseSQL . prepSQL $ t)

testToken :: IO ()
testToken = withConnection $ \conn -> do
    quickCheck prop_parseToken
    quickCheck $ prop_insertToken conn
    quickCheck $ prop_selectToken conn

instance Arbitrary User where
    arbitrary = do
        u <- randomString
        n <- randomString
        sn <- randomString
        return $ User u n sn

insertUser :: IConnection c => c -> User -> IO Integer
insertUser c = persist c (SQLExpr "insert into User (id, name, screen_name) values (?, ?, ?)" [])

prop_parseUser :: User -> Bool
prop_parseUser u = Just u == (parseSQL . prepSQL $ u)

prop_insertUser :: IConnection c => c -> User -> Property
prop_insertUser = prop $ \c u -> liftM (==1) $ insertUser c u

prop_selectUser :: IConnection c => c -> User -> Property
prop_selectUser = undefined

testUser :: IO ()
testUser = withConnection $ \conn -> do
        quickCheck prop_parseUser
        quickCheck $ prop_insertUser conn
