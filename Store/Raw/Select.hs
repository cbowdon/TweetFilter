-- | Raw SQL query strings
module Store.Raw.Select where

token :: String
token = " * from Token where access_token = ? and token_type = ?"

user :: String
user = " * \
            \from    User \
            \where   id = ? \
                \and name = ? \
                \and screen_name = ?"

tweet :: String
tweet = " * \
            \from    Tweet as T \
            \join    User as U \
                \on  T.user_id = U.id \
            \where   T.text = ? \
                \and U.id = ? \
                \and U.name = ? \
                \and U.screen_name = ?"
