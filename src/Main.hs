{-#LANGUAGE OverloadedStrings #-}

module Main where
import qualified TwitterTypes as TT
import SqlTypes

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import Data.Aeson
import Data.Char (isSpace)
import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad

dbFile :: String
dbFile = "tweets.db"

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

getLatestTweets :: IO (Either String TT.TwitterSearch)
getLatestTweets = do
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
        return ((eitherDecode response) :: (Either String TT.TwitterSearch))

statusToTweetFields :: TT.Status -> TweetFields
statusToTweetFields status = (TweetFields (TT.id status) (TT.screen_name (TT.user status)) (TT.text status))

createTableIfNotExists :: IO ()
createTableIfNotExists = DB.withConnection dbFile
  (\conn ->
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS tweets (id INTEGER PRIMARY KEY, tweet_id INT, author VARCHAR(50), text VARCHAR(140))")

writeTweetsToDB :: [TT.Status] -> IO ()
writeTweetsToDB tweets =
  DB.withConnection dbFile (executeStatements tweets)
  where
    executeStatements [] _ = return ()
    executeStatements (status:rst) conn = do
      DB.execute conn "INSERT INTO tweets (tweet_id,author,text) values (?,?,?)" (statusToTweetFields status)
      executeStatements rst conn

main :: IO ()
main = do
  createTableIfNotExists
  tweetResult <- getLatestTweets
  case tweetResult of
    Left err -> putStrLn err
    Right tweets -> writeTweetsToDB $ TT.statuses tweets
