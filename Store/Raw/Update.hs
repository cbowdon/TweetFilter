-- | Raw SQL update strings
module Store.Raw.Update where

markTweet :: String
markTweet = "update Tweet \
            \set spam = 1 \
            \where id = ?;"

markUser :: String
markUser = "update User \
            \set spammer = 1 \
            \where id = ?;"
