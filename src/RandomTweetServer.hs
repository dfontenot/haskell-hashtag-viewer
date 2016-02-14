{-#LANGUAGE OverloadedStrings, DeriveGeneric #-}

module RandomTweetServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Database.SQLite.Simple as DB
import GHC.Generics
import Data.Aeson
import Network.HTTP.Server
import Network.URL
import Control.Monad
import Control.Monad.Trans.Maybe

dbFile :: String
dbFile = "tweets.db"

headers :: [Header]
headers = [Header HdrPragma "no-cache"
          , Header HdrContentType "application/json; charset=utf-8"
          , Header HdrConnection "close"
          , Header HdrServer "RandomTweetServer"]

data RandomTweetResponse =
  RandomTweetResponse { screenName :: T.Text
                      , userName :: T.Text
                      , tweet :: T.Text
                      , img_ext :: T.Text
                      , image :: T.Text } deriving (Show, Generic)
instance ToJSON RandomTweetResponse

-- tweet_id, screenName, userName, tweet
type TweetRow = (T.Text, T.Text, T.Text, T.Text)

type DBMonad = MaybeT IO

doRespond :: Maybe RandomTweetResponse -> IO (Response BL.ByteString)
doRespond (Just tweetResp) = return $ Response (2,0,0) "Ok" headers (encode tweetResp)
doRespond Nothing = return $ Response (5,0,0) "Internal Server Error" headers jsonMsg
  where
    jsonMsg = encode $ object [ "error_message" .= ("Internal Server Error" :: T.Text) ]

getRandomTweetResponse :: DB.Connection -> DBMonad RandomTweetResponse
getRandomTweetResponse conn = do
  (tweet_id,screen_name,user_name,tweet_) <- getRandomTweetText conn
  (base64Image, ext) <- getImage conn tweet_id
  return $ RandomTweetResponse screen_name user_name tweet_ ext base64Image

getImage :: DB.Connection -> T.Text -> DBMonad (T.Text, T.Text)
getImage conn tweet_id = MaybeT $ do
  -- TEXT column types must be fetched as a ByteString
  res <- DB.query conn "SELECT image, img_ext FROM images WHERE tweet_id = ?" (DB.Only tweet_id) :: IO [(B.ByteString, T.Text)]
  return $ case res of
    [(image, ext)] -> Just $ (E.decodeUtf8 image, ext)
    _ -> Nothing

getRandomTweetText :: DB.Connection -> DBMonad TweetRow
getRandomTweetText conn = MaybeT $ do
  res <-
    DB.query_ conn "SELECT tweet_id, user_name, screen_name, text FROM tweets ORDER BY RANDOM() LIMIT 1" :: IO [TweetRow]
  return $ case res of
    [vals] -> Just vals
    _ -> Nothing

handler :: URL -> Request BL.ByteString -> IO (Response BL.ByteString)
handler url req = DB.withConnection dbFile handleWithConnection
  where
    handleWithConnection conn = case rqMethod req of
      GET -> (runMaybeT (getRandomTweetResponse conn) ) >>= doRespond
      _ -> return $ err_response NotImplemented

main :: IO ()
main = server handler'
  where
    handler' _ url req = handler url req
