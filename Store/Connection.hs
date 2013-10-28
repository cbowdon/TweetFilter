-- | Connection utilities for the persistence layer
module Store.Connection
( withConnection
) where

import Database.HDBC
import Database.HDBC.Sqlite3

db :: String
db = "tweets.db"

-- | Perform the action with a connection, handle any errors by printing to stdout
withConnection :: (Connection -> IO ()) -> IO ()
withConnection f = handleSql catcher $ do
    conn <- connectSqlite3 db
    a <- f conn
    disconnect conn
    return a

catcher :: SqlError -> IO ()
catcher = print . ("Unhandled exception: "++) . show
