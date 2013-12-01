-- | Raw SQL update strings
module Tweet.Store.Raw.Update where

markTweet :: String
markTweet = "update Tweet \
            \set spam = ? \
            \where text = ? \
            \and user_id = ?;"

markUser :: String
markUser = "update User \
            \set spammer = ? \
            \where id = ?;"
