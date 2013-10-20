module Store.Test where

import Store
import Store.Connection
import TwitterTypes
import Control.Monad

test :: IO ()
test = do
    jsonStr <- getFileContents "/home/chris/Tweet/auth/bear_token.json"
