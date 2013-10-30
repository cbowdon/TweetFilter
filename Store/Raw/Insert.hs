-- | Raw SQL insert commands
module Store.Raw.Insert where

token :: String
token = "insert into Token (access_token, token_type) values (?, ?)"

user :: String
user = "insert into User (id, name, screen_name) values (?, ?, ?)"

tweet :: String
tweet = "insert into Tweet (text, user_id) values (?, ?)"
