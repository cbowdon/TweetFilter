module Store.Connection
( db
, withConnection
) where

import Database.HDBC
import Database.HDBC.Sqlite3

db :: String
db = "tweets.db"

withConnection :: (Connection -> IO ()) -> IO ()
withConnection f = handleSql catcher $ do
    conn <- connectSqlite3 db
    a <- f conn
    disconnect conn
    return a

catcher :: SqlError -> IO ()
catcher = print . ("Unhandled exception: "++) . show
