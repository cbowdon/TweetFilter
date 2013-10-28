module Store
( ToSQL(..)
, FromSQL(..)
, persist
, retrieve
, SQLExpr(..)
) where

import Database.HDBC

data SQLExpr = SQLExpr  { statement :: String
                        , parameters :: [SqlValue] }

class ToSQL a where
    prepSQL :: a -> [SqlValue]

class FromSQL a where
    parseSQL :: [SqlValue] -> Maybe a
    parseSQL _ = Nothing

persist :: (IConnection c, ToSQL a) => c -> SQLExpr -> a -> IO Integer
persist conn (SQLExpr stmt pms) a =
    withTransaction conn $ \c -> run c stmt $ prepSQL a ++ pms

retrieve :: (IConnection c, FromSQL a) => c -> SQLExpr -> IO (Maybe [a])
retrieve conn (SQLExpr stmt pms) =
        withTransaction conn $ \c -> do
            items <- quickQuery' c stmt pms
            return . mapM parseSQL $ items
