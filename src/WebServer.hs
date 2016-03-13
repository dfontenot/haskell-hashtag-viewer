{-#LANGUAGE OverloadedStrings #-}

module WebServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import Network.HTTP.Server
import Network.URL
import Control.Monad
import System.Directory

webRoot :: String
webRoot = "/Users/david/workspace"

data FileResponse =
  PermissionDenied String
  | FileNotFound String
  | FileOK String

headers :: [Header]
headers = [Header HdrPragma "no-cache"
          , Header HdrContentType "text/html; charset=utf-8"
          , Header HdrConnection "close"
          , Header HdrServer "HashtagViewerWebServer"]

dumpUrl :: URL -> IO (Response BL.ByteString)
dumpUrl url = return $ Response (2,0,0) "Ok" headers $ (C8L.pack . show) url

respondForFile :: FileResponse -> IO (Response BL.ByteString)
respondForFile (FileOK path) = (BL.readFile path) >>= (\contents -> return (Response (2,0,0) "Ok" headers contents))
respondForFile (PermissionDenied path) = return $ Response (4,0,3) "Permission denied" headers $ (C8L.pack . show) path
respondForFile (FileNotFound path) = return $ Response (4,0,4) "File not found" headers $ (C8L.pack . show) path

getResponseForFile :: String -> IO FileResponse
getResponseForFile filePath = do
  exists <- doesFileExist filePath
  if exists
     then (getPermissions filePath) >>=
          (\perms -> if readable perms
                        then return $ FileOK filePath
                        else return $ PermissionDenied filePath)
     else return $ FileNotFound filePath

doRespond :: URL -> IO (Response BL.ByteString)
doRespond url = (getResponseForFile filePath) >>= respondForFile
  where filePath = webRoot ++ (url_path url)

handler :: URL -> Request BL.ByteString -> IO (Response BL.ByteString)
handler url req = case rqMethod req of
  GET -> doRespond url
  _ -> return $ err_response NotImplemented

main :: IO ()
main = server handler'
  where
    handler' _ url req = handler url req
