{-#LANGUAGE OverloadedStrings, DeriveGeneric #-}

module RandomTweetServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Database.SQLite.Simple as DB
import qualified Data.List as L
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import Network.URI
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import System.Environment

import SimpleArgvParser

dbFile :: String
dbFile = "tweets.db"

headers :: [Header]
headers = [Header HdrPragma "no-cache"
          , Header HdrConnection "close"
          , Header HdrServer "RandomTweetServer"]

jsonHeaders :: [Header]
jsonHeaders = (mkHeader HdrContentType "application/json; charset=utf-8"):headers

allowGetForOrigin :: [Header] -> String -> [Header]
allowGetForOrigin headers origin = (Header (HdrCustom "Access-Control-Allow-Origin") origin)
  :(Header (HdrCustom "Access-Control-Allow-Methods") "GET")
  :headers

data RandomTweetResponse =
  RandomTweetResponse { screenName :: T.Text
                      , userName :: T.Text
                      , tweet :: T.Text
                      , mimeType :: T.Text
                      , image :: T.Text } deriving (Show, Generic)
instance ToJSON RandomTweetResponse

-- tweet_id, screenName, userName, tweet
type TweetRow = (T.Text, T.Text, T.Text, T.Text)

type DBMonad = MaybeT IO

-- TODO: move string arg to reader
doRespond :: String -> Maybe RandomTweetResponse -> IO (Response BL.ByteString)
doRespond origin resp = return (case resp of
                                   Just tweetResp -> Response (2,0,0) "Ok" myHeaders (encode tweetResp)
                                   Nothing -> let jsonMsg = encode $ object [ "error_message" .= ("Internal Server Error" :: T.Text) ] in
                                     Response (5,0,0) "Internal Server Error" myHeaders jsonMsg)
  where
    myHeaders = allowGetForOrigin jsonHeaders origin

getRandomTweetResponse :: DB.Connection -> DBMonad RandomTweetResponse
getRandomTweetResponse conn = do
  (tweet_id,screen_name,user_name,tweet_) <- getRandomTweetText conn
  (base64Image, mimeType) <- getImage conn tweet_id
  return $ RandomTweetResponse screen_name user_name tweet_ mimeType base64Image

getImage :: DB.Connection -> T.Text -> DBMonad (T.Text, T.Text)
getImage conn tweet_id = MaybeT $ do
  -- TEXT column types must be fetched as a ByteString
  res <- DB.query conn "SELECT image, mime_type FROM images WHERE tweet_id = ?" (DB.Only tweet_id) :: IO [(B.ByteString, T.Text)]
  return $ case res of
    [(image, mime_type)] -> Just $ (E.decodeUtf8 image, mime_type)
    _ -> Nothing

getRandomTweetText :: DB.Connection -> DBMonad TweetRow
getRandomTweetText conn = MaybeT $ do
  res <-
    DB.query_ conn "SELECT tweet_id, user_name, screen_name, text FROM tweets ORDER BY RANDOM() LIMIT 1" :: IO [TweetRow]
  return $ case res of
    [vals] -> Just vals
    _ -> Nothing

-- TODO: switch args to match other functions
handleWithConnection :: String -> DB.Connection -> IO (Response BL.ByteString)
handleWithConnection origin conn = (runMaybeT (getRandomTweetResponse conn) ) >>= doRespond origin

-- TODO: move to separate lib
originAcceptable :: String -> Bool
originAcceptable origin = case getUriRegName of
  Just name -> name == "localhost"
  Nothing -> False
  where
    getUriRegName = fmap uriRegName $ (parseURI origin) >>= uriAuthority

getOriginIfSetAndAllowed :: [Header] -> Maybe String
getOriginIfSetAndAllowed headers = fmap hdrValue foundHeader
  where
    foundHeader = L.find (\header -> (hdrName header) == (HdrCustom "Origin") && originAcceptable (hdrValue header)) headers

-- TODO: pass in origin via reader
handler :: URL -> Request BL.ByteString -> IO (Response BL.ByteString)
handler url req = case rqMethod req of
  GET -> case getOriginIfSetAndAllowed (rqHeaders req) of
    Just origin -> DB.withConnection dbFile $ handleWithConnection origin
    Nothing -> return $ Response (4,0,3) "Unacceptable origin" headers ""
  _ -> return $ err_response NotImplemented

main :: IO ()
main = do
  args <- getArgs

  case pairArguments args of
    Just argMap -> serverWith (serverConfig (Map.lookup "bind" argMap)) handler'
    Nothing -> putStrLn usage
  where
    usage = "./RandomTweetServer [--bind <address>]"
    handler' _ url req = handler url req
    serverConfig maybeAddr = Config stdLogger (fromMaybe "localhost" maybeAddr) 8000
