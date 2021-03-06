-- | Persistence layer
module Tweet.Store
( -- * Types
SQLExpr(..)
-- * Classes
, IdSQL(..)
, ToSQL(..)
, FromSQL(..)
-- * Functions
, modify
, persist
, retrieve
-- * Re-exported functions
, toSql
, fromSql
) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Database.HDBC

-- | A SQL expression (query, insert, update) with parameters
data SQLExpr = SQLExpr  { statement :: String
                        , parameters :: [SqlValue] }

-- | Class for datatypes that can be uniquely identified in SQL
class IdSQL a where
    idSQL :: a -> [SqlValue]

-- | Class for datatypes can be converted to SQL values and inserted into database
class (IdSQL a) => ToSQL a where
    prepSQL :: a -> [SqlValue]
    insert :: IConnection c => a -> ReaderT c IO Integer

-- | Class for datatypes can be converted from SQL values and retrieve from database
class (IdSQL a) => FromSQL a where
    parseSQL :: [SqlValue] -> Maybe a
    select :: IConnection c => a -> ReaderT c IO [a]

-- | General function for persisting data - returns count of rows affected
persist :: (IConnection c, ToSQL a) => SQLExpr -> a -> ReaderT c IO Integer
persist (SQLExpr stmt pms) a = do
        conn <- ask
        liftIO $ withTransaction conn $ \c -> run c stmt $ prepSQL a ++ pms

-- | General function for modifying persisted data - returns count of rows affected
modify :: (IConnection c, ToSQL a) => SQLExpr -> a -> ReaderT c IO Integer
modify (SQLExpr stmt pms) a = do
        conn <- ask
        liftIO $ withTransaction conn $ \c -> run c stmt $ pms ++ idSQL a

-- | General function for retrieving data - returns list of possible values
retrieve :: (IConnection c, FromSQL a) => SQLExpr -> ReaderT c IO [a]
retrieve (SQLExpr stmt pms) = do
    conn <- ask
    raw <- liftIO $ withTransaction conn $ \c -> quickQuery' c stmt pms
    let items = mapM parseSQL raw
    case items of
        Just v  -> return v
        Nothing -> return []
