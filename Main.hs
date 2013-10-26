import Control.Monad
import Data.Conduit
import Network.HTTP.Conduit
import TwitterTypes
import Store
import Store.Connection
import Store.Test
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    token <- BL.readFile "/home/chris/Tweet/auth/bear_token.json"
    content <- BL.readFile "/home/chris/Tweet/data/example.json"
    html <- simpleHttp "https://www.startpage.com/"
    putStrLn . take 100 . show $ html
    print (eitherDecode token :: Either String Token)
    print (eitherDecode content :: Either String Tweets)
