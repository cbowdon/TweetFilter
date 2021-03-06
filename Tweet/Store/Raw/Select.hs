-- | Raw SQL query strings
module Tweet.Store.Raw.Select where

token :: String
token = "select access_token, token_type from Token where access_token = ? and token_type = ?;"

user :: String
user = "select id, name, screen_name, spammer \
            \from    User \
            \where   id = ?;"

tweet :: String
tweet = "select text, spam, user_id, name, screen_name, spammer \
            \from    Tweet as T \
            \join    User as U \
                \on  T.user_id = U.id \
            \where   T.text = ? \
                \and U.id = ?;"

loadTweets :: String
loadTweets = "select text, spam, user_id, name, screen_name, spammer \
                \from    Tweet as T \
                \join    User as U \
                    \on  T.user_id = U.id;"
