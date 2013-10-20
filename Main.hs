import Control.Monad
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import TwitterTypes
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
{- import qualified Data.ByteString.Lazy.Char8 as BS (pack) -}
-- TODO cleaning

main :: IO ()
main = do
    token <- BL.readFile "/home/chris/Tweet/auth/bear_token.json"
    content <- BL.readFile "/home/chris/Tweet/data/example.json"
    html <- simpleHttp "https://www.startpage.com/"
    putStrLn . take 100 . show $ html
    print (eitherDecode token :: Either String Token)
    print (eitherDecode content :: Either String Tweets)
