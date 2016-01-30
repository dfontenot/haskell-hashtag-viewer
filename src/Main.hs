{-#LANGUAGE OverloadedStrings #-}

module Main where
import TwitterTypes

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Char (isSpace)
import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad

-- https://dev.twitter.com/rest/public/search
searchQuery :: String
searchQuery = "https://api.twitter.com/1.1/search/tweets.json?q=%23cats"

myOAuth :: B.ByteString -> B.ByteString -> OAuth
myOAuth consumerKey consumerSecret =
  newOAuth { oauthConsumerKey = consumerKey,
             oauthConsumerSecret = consumerSecret }

getResponse :: Request -> IO BL.ByteString
getResponse request = withManager tlsManagerSettings
  (\manager ->
    (liftM responseBody) $ httpLbs request manager)

readFileStripWhitespace :: String -> IO B.ByteString
readFileStripWhitespace fn = liftM BC.pack filteredContents
  where
    filteredContents = liftM (filter (not . isSpace)) $ readFile fn

main :: IO ()
main = do
  consumerKey <- readFileStripWhitespace "private/consumer.key"
  consumerSecret <- readFileStripWhitespace "private/consumer.secret"
  accessToken <- readFileStripWhitespace "private/access.token"
  accessSecret <- readFileStripWhitespace "private/access.secret"

  myRequest <- parseUrl searchQuery

  let oauthConsumer = myOAuth consumerKey consumerSecret in
    let creds = newCredential accessToken accessSecret in
      do
        signedRequest <- signOAuth oauthConsumer creds myRequest
        response <- getResponse signedRequest
        let jsonTweets = (eitherDecode response) :: (Either String TwitterSearch) in
          case jsonTweets of
            Left err -> putStrLn err
            Right search -> T.putStrLn $ text (head (statuses search))
