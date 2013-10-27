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

insertion :: IConnection c => c -> Token -> IO Bool
insertion c t = liftM (==1) $ insertToken c t

selection :: IConnection c => c -> Token -> IO Bool
selection c t = do
    _ <- insertToken c t
    m <- selectToken c t
    case m of
        Just t' -> return $ all (t==) t'
        o       -> return False

prop_selection :: IConnection c => c -> Token -> Property
prop_selection c t = monadicIO $ do
    result <- run $ do
        x <- selectToken c t
        selection c t
    assert result

prop_insertion :: IConnection c => c -> Token -> Property
prop_insertion c t = monadicIO $ do
    result <- run $ insertion c t
    assert result

prop_parse :: Token -> Bool
prop_parse t = Just t == (parseSQL . prepSQL $ t)

testToken :: IO ()
testToken = withConnection $ \conn -> do
    print "PARSING"
    quickCheck prop_parse
    print "INSERTION"
    quickCheck $ prop_insertion conn
    print "SELECTION"
    quickCheck $ prop_selection conn
