{-#LANGUAGE OverloadedStrings #-}

module WebServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified System.FilePath as FP
import Data.String.Conversions
import Data.Word
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import Network.Mime
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import System.Directory
import System.Environment

import SimpleArgvParser

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
respondForFile (PermissionDenied path) = return $ Response (4,0,3) "Permission denied" headers (cs path)
respondForFile (FileNotFound path) = return $ Response (4,0,4) "File not found" headers (cs path)
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

makeServerConfig :: Map.Map String String -> Config
makeServerConfig argMap = Config stdLogger "localhost" port
  where
    port = case Map.lookup "port" argMap of
      Just port -> fromIntegral (read port :: Integer)
      Nothing -> 8001

main :: IO ()
main = do
  args <- getArgs

  -- TODO: could be made cleaner
  case pairArguments args of
    Just argMap -> case Map.lookup "root" argMap of
      Just web_root -> serverWith (makeServerConfig argMap) $ handler' web_root
      Nothing -> putStrLn usage
    Nothing -> putStrLn usage
  where
    handler' webRoot _ url req = runReaderT (handler url req) (ServerSettings webRoot)
    usage = "./WebServer --root <web_root> [--port <port>]"
