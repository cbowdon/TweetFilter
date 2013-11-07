-- | Persistence layer
module Store
( -- * Types
SQLExpr(..)
-- * Classes
, ToSQL(..)
, FromSQL(..)
-- * Functions
, persist
, retrieve
-- * Re-exported functions
, toSql
, fromSql
) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Database.HDBC

-- | A SQL expression (query, insert, update) with parameters
data SQLExpr = SQLExpr  { statement :: String
                        , parameters :: [SqlValue] }

-- | Class for datatypes can be converted to SQL values and inserted into database
class ToSQL a where
    prepSQL :: a -> [SqlValue]
    insert :: IConnection c => c -> a -> IO Integer

-- | Class for datatypes can be converted from SQL values and retrieve from database
class FromSQL a where
    parseSQL :: [SqlValue] -> Maybe a
    select :: IConnection c => c -> a -> IO (Maybe [a])

-- | General function for persisting data - returns count of rows affected
persist :: (IConnection c, ToSQL a) => c -> SQLExpr -> a -> IO Integer
persist conn (SQLExpr stmt pms) a =
    withTransaction conn $ \c -> run c stmt $ prepSQL a ++ pms

-- | General function for retrieving data - returns list of possible values
retrieve :: (IConnection c, FromSQL a) => c -> SQLExpr -> IO (Maybe [a])
retrieve conn (SQLExpr stmt pms) =
        withTransaction conn $ \c -> do
            items <- quickQuery' c stmt pms
            return . mapM parseSQL $ items

persist' :: (IConnection c, ToSQL a) => SQLExpr -> a -> ReaderT c IO Integer
persist' (SQLExpr stmt pms) a = do
        conn <- ask
        liftIO $ withTransaction conn $ \c -> run c stmt $ prepSQL a ++ pms

retrieve' :: (IConnection c, FromSQL a) => c -> SQLExpr -> MaybeT IO [a]
retrieve' conn (SQLExpr stmt pms) =
    MaybeT $ withTransaction conn $ \c -> do
            items <- quickQuery' c stmt pms
            return . mapM parseSQL $ items
