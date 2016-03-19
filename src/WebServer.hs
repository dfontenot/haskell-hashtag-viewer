{-#LANGUAGE OverloadedStrings #-}

module WebServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified System.FilePath as FP
import Data.String.Conversions
import Network.HTTP.Server
import Network.URL
import Network.Mime
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import System.Directory
import System.Environment

data ServerSettings = ServerSettings
                      { webRoot :: String
                      }

data FileResponse =
  PermissionDenied String
  | FileNotFound String
  | FileOK String

type ServerMonad = ReaderT ServerSettings IO

headers :: [Header]
headers = [Header HdrPragma "no-cache"
          , Header HdrConnection "close"
          , Header HdrServer "HashtagViewerWebServer"]

respondForFile :: FileResponse -> IO (Response BL.ByteString)
respondForFile (PermissionDenied path) = return $ Response (4,0,3) "Permission denied" headers $ (C8L.pack . show) path
respondForFile (FileNotFound path) = return $ Response (4,0,4) "File not found" headers $ (C8L.pack . show) path
respondForFile (FileOK path) = do
  contents <- BL.readFile path
  return $ Response (2,0,0) "Ok" modifiedHeaders contents
    where
      mimeTypeHeader = Header HdrContentType $ cs $ defaultMimeLookup (cs path)
      modifiedHeaders = mimeTypeHeader:headers

getResponseForFile :: String -> IO FileResponse
getResponseForFile filePath = do
  exists <- doesFileExist filePath
  if exists
     then (getPermissions filePath) >>=
          (\perms -> if readable perms
                        then return $ FileOK filePath
                        else return $ PermissionDenied filePath)
     else return $ FileNotFound filePath

doRespond :: URL -> ServerMonad (Response BL.ByteString)
doRespond url = do
  root <- asks webRoot
  file <- liftIO $ getResponseForFile (root ++ (url_path url))
  liftIO $ respondForFile file

handler :: URL -> Request BL.ByteString -> ServerMonad (Response BL.ByteString)
handler url req = case rqMethod req of
  GET -> doRespond url
  _ -> return $ err_response NotImplemented

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
    then putStrLn "Specify a web root"
    else server $ handler' (head args)
  where
    handler' webRoot _ url req = runReaderT (handler url req) (ServerSettings webRoot)
