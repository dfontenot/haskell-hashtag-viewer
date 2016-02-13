{-#LANGUAGE OverloadedStrings, DeriveGeneric #-}

module RandomTweetServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import System.IO.Error
import GHC.Generics
import Data.Aeson
import Network.HTTP.Server
import Network.URL
import Control.Monad

dbFile :: String
dbFile = "tweets.db"

headers :: [Header]
headers = [Header HdrPragma "no-cache"
          , Header HdrContentType "application/json"
          , Header HdrConnection "close"
          , Header HdrServer "RandomTweetServer"]

raiseUserIOError :: String -> IO a
raiseUserIOError str = (ioError . userError) str

data RandomTweetResponse =
  RandomTweetResponse { screenName :: T.Text
                      , userName :: T.Text
                      , tweet :: T.Text
                      , image :: T.Text } deriving (Show, Generic)
instance ToJSON RandomTweetResponse

-- tweet_id, screenName, userName, tweet
type TweetRow = (T.Text, T.Text, T.Text, T.Text)

getImage :: DB.Connection -> TweetRow -> IO RandomTweetResponse
getImage conn (tweet_id,screen_name,user_name,tweet_) = do
  res <- DB.query conn "SELECT image FROM images WHERE tweet_id = ?" (DB.Only tweet_id) :: IO [DB.Only T.Text]
  case res of
    [] -> raiseUserIOError "No image associated with tweet_id"
    ((DB.Only image):[]) -> return $ RandomTweetResponse screen_name user_name tweet_ image
    _ -> raiseUserIOError "Something went wrong with the query"

getRandomTweetText :: DB.Connection -> IO TweetRow
getRandomTweetText conn = do
  res <-
    DB.query_ conn "SELECT tweet_id, user_name, screen_name, text FROM tweets ORDER BY RANDOM() LIMIT 1" :: IO [TweetRow]
  case res of
    [] -> raiseUserIOError "No tweets stored in database"
    ((tweet_id, user_name, screen_name, tweet_):[]) -> return (tweet_id, screen_name, user_name, tweet_)
    _ -> raiseUserIOError "Something went wrong with the query"

getRandomTweet :: DB.Connection -> IO (Either String RandomTweetResponse)
getRandomTweet conn = catchIOError (chainedDBCalls conn) (\err -> return $ Left "Everything failed")
  where
    chainedDBCalls conn = do
      tr <- getRandomTweetText conn
      resp <- getImage conn tr
      return $ Right resp

respondOk :: RandomTweetResponse -> Response BL.ByteString
respondOk tweetResp = Response (2,0,0) "yay" headers (encode tweetResp)

respondError :: String -> Response BL.ByteString
respondError msg = Response (5,0,0) "sad" headers jsonMsg
  where
    jsonMsg = encode $ object [ "error_message" .= msg ]

doRespond :: DB.Connection -> IO (Response BL.ByteString)
doRespond conn = do
  randomTweetRes <- getRandomTweet conn
  return $ case randomTweetRes of
    Left msg -> respondError msg
    Right result -> respondOk result

handler :: URL -> Request BL.ByteString -> IO (Response BL.ByteString)
handler url req = DB.withConnection dbFile handleWithConnection
  where
    handleWithConnection conn = case rqMethod req of
      GET -> doRespond conn
      _ -> return $ err_response NotImplemented

main :: IO ()
main = server handler'
  where
    handler' _ url req = handler url req
