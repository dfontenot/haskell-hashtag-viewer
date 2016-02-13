{-#LANGUAGE OverloadedStrings #-}

module TwitterPoller where
import qualified TwitterTypes as TT
import SqlTypes

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as BB
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

-- source: http://stackoverflow.com/a/8552723/854854
blToStrict :: BL.ByteString -> B.ByteString
blToStrict = B.concat . BL.toChunks

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
statusToTweetFields status = (TweetFields (TT.id_str status) (TT.name (TT.user status)) (TT.screen_name (TT.user status)) (TT.text status))

createTablesIfNotExists :: IO ()
createTablesIfNotExists = DB.withConnection dbFile
  (\conn ->
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS tweets (tweet_id VARCHAR(100) PRIMARY KEY, user_name VARCHAR(50), screen_name VARCHAR(50), text VARCHAR(140))"
    >> DB.execute_ conn "CREATE TABLE IF NOT EXISTS images (id INTEGER PRIMARY KEY, tweet_id VARCHAR(100), image TEXT)")

getImagesDataFromMedia :: [TT.Media] -> IO [B.ByteString]
getImagesDataFromMedia media = mapM fetchHTTPSUrl img_entities
  where
    fetchHTTPSUrl url = (parseUrl (T.unpack url)) >>= (\req -> getResponse req) >>= (\resp -> return $ (BB.encode . blToStrict) resp)
    img_entities = map (\m -> TT.media_url_https m) $ filter (\m -> (TT.media_type m) == "photo") media

insertTweet :: TT.Status -> DB.Connection -> IO ()
insertTweet status conn = do
  case (TT.media . TT.entities) status of
    Just media -> do
      DB.execute conn "INSERT INTO tweets VALUES (?,?,?,?)" (statusToTweetFields status)
      images <- getImagesDataFromMedia media
      mapM_ (\i -> DB.execute conn "INSERT INTO images (tweet_id, image) VALUES (?, ?)" (TT.id_str status, i)) images
    Nothing -> return ()

writeTweetsToDB :: [TT.Status] -> IO ()
writeTweetsToDB tweets =
  DB.withConnection dbFile (executeStatements tweets)
  where
    executeStatements [] _ = return ()
    executeStatements (status:rst) conn = do
      -- only insert unique tweets
      res <- (DB.query conn "SELECT tweet_id FROM tweets WHERE tweet_id = ? LIMIT 1" (DB.Only (TT.id_str status))) :: IO [DB.Only T.Text]
      case res of
        [] -> (insertTweet status conn) >> executeStatements rst conn
        _ -> executeStatements rst conn

main :: IO ()
main = do
  createTablesIfNotExists
  tweetResult <- getLatestTweets
  case tweetResult of
    Left err -> putStrLn err
    Right tweets -> writeTweetsToDB $ TT.statuses tweets
