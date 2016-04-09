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
import qualified Data.Text.Encoding as E
import qualified Data.Map as Map
import qualified Database.SQLite.Simple as DB
import Data.Aeson
import Data.Char (isSpace)
import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Mime
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
    >> DB.execute_ conn "CREATE TABLE IF NOT EXISTS images (id INTEGER PRIMARY KEY, tweet_id VARCHAR(100), mime_type VARCHAR(4), image TEXT)")

getImagesDataFromMedia :: [TT.Media] -> IO [(T.Text, B.ByteString)]
getImagesDataFromMedia media = mapM fetchHTTPSUrl (TT.getHTTPSImagesUrls media)
  where
    fetchHTTPSUrl url = do
      req <- parseUrl $ T.unpack url
      resp <- getResponse req
      let encoded = (BB.encode . blToStrict) resp in
        return (url, encoded)

-- TODO: reader-ify DB connection
insertTweet :: TT.Status -> DB.Connection -> IO ()
insertTweet status conn = do
  case (TT.media . TT.entities) status of
    Just media -> do
      DB.execute conn "INSERT INTO tweets VALUES (?,?,?,?)" (statusToTweetFields status)
      images <- getImagesDataFromMedia media
      mapM_ (\(url,img) -> insertImageForTweet conn (TT.id_str status) (getMimeTypeFromUrl url) img) images
    Nothing -> return ()

insertImageForTweet :: DB.Connection -> T.Text -> Maybe T.Text -> B.ByteString -> IO ()
insertImageForTweet _ _ Nothing _ = return ()
insertImageForTweet conn id (Just mimeType) img =
  DB.execute conn "INSERT INTO images (tweet_id, mime_type, image) VALUES (?,?,?)" (id, mimeType, img)

getMimeTypeFromUrl :: T.Text -> Maybe T.Text
getMimeTypeFromUrl url = fmap E.decodeUtf8 $ Map.lookup ext defaultMimeMap
  where
    ext = T.takeWhileEnd(/= '.') url

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
