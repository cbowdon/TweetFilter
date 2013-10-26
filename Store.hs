module Store
( ToSQL(..)
, FromSQL(..)
, persist
, retrieve
, SQLExpr(..)
) where

import Database.HDBC
import Store.Connection

data SQLExpr = SQLExpr  { statement :: String
                        , parameters :: [SqlValue] }

class ToSQL a where
    prepSQL :: a -> [SqlValue]

class FromSQL a where
    parseSQL :: [SqlValue] -> Maybe a
    parseSQL _ = Nothing

persist :: (IConnection c, ToSQL a) => c -> SQLExpr -> a -> IO Integer
persist conn (SQLExpr stmt params) item =
    withTransaction conn $ \c -> run c stmt $ prepSQL item ++ params

retrieve :: (IConnection c, FromSQL a) => c -> SQLExpr -> IO (Maybe [a])
retrieve conn (SQLExpr stmt params) =
    withTransaction conn $ \c -> do
        items <- quickQuery' c stmt params
        return . mapM parseSQL $ items
