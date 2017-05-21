{-#LANGUAGE OverloadedStrings #-}

module WebServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified System.FilePath as FP
import qualified Network.URL as URL
import Data.String.Conversions
import Data.Maybe
import Data.Word
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.Mime
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import System.Directory
import System.Environment

import SimpleArgvParser

data ServerSettings = ServerSettings { webRoot :: String, port :: Int }

data FileResponse =
  PermissionDenied String
  | FileNotFound String
  | FileOK String
  | RedirectToIndex

type ServerMonad = ReaderT ServerSettings IO

headers :: [Header]
headers = [Header HdrPragma "no-cache"
          , Header HdrConnection "close"
          , Header HdrServer "HashtagViewerWebServer"]

redirectHeaders :: Int -> [Header]
redirectHeaders port = [Header HdrPragma "no-cache"
                       , Header HdrConnection "keep-alive"
                       , Header HdrLocation $ "http://localhost:" ++ (show port) ++ "/index.html"
                       , Header HdrServer "HashtagViewerWebServer"]

respondForFile :: FileResponse -> ServerMonad (Response BL.ByteString)
respondForFile (PermissionDenied path) = return $ Response (4,0,3) "Permission denied" headers $ BL.append "Permission denied: " (cs path)
respondForFile (FileNotFound path) = return $ Response (4,0,4) "File not found" headers $ BL.append "File not found: " (cs path)
respondForFile RedirectToIndex = do
  port <- asks port
  return $ Response (3,0,1) "Moved Permanently" (redirectHeaders port) "Redirect"
respondForFile (FileOK path) = do
  contents <- liftIO $ BL.readFile path
  return $ Response (2,0,0) "Ok" modifiedHeaders contents
    where
      mimeTypeHeader = Header HdrContentType $ cs $ defaultMimeLookup (cs path)
      modifiedHeaders = mimeTypeHeader:headers

getResponseForFile :: String -> ServerMonad FileResponse
getResponseForFile urlPath = do
  root <- asks webRoot
  let filePath = root ++ urlPath in
      if urlPath == "" then return RedirectToIndex else getFileResponse filePath
  where
    getFileResponse filePath = liftIO $ do
      exists <- doesFileExist filePath
      if exists
         then (getPermissions filePath) >>=
              (\perms -> if readable perms
                            then return $ FileOK filePath
                            else return $ PermissionDenied filePath)
         else return $ FileNotFound filePath

doRespond :: URL.URL -> ServerMonad (Response BL.ByteString)
doRespond url = do
  file <- getResponseForFile $ URL.url_path url
  respondForFile file

handler :: URL.URL -> Request BL.ByteString -> ServerMonad (Response BL.ByteString)
handler url req = case rqMethod req of
  GET -> doRespond url
  _ -> return $ err_response NotImplemented

getPortFromArgs :: (Num a) => Map.Map String String -> a
getPortFromArgs argMap = case Map.lookup "port" argMap of
                           Just port -> fromIntegral (read port :: Integer)
                           Nothing -> 8001

getBindAddressFromArgs :: Map.Map String String -> String
getBindAddressFromArgs argMap = fromMaybe "localhost" $ Map.lookup "bind" argMap

makeServerConfig :: Map.Map String String -> Config
makeServerConfig argMap = Config stdLogger (getBindAddressFromArgs argMap) (getPortFromArgs argMap)

main :: IO ()
main = do
  args <- getArgs

  -- TODO: could be made cleaner
  case pairArguments args of
    Just argMap -> case Map.lookup "root" argMap of
      Just web_root -> serverWith (makeServerConfig argMap) $ handler' web_root (getPortFromArgs argMap)
      Nothing -> putStrLn usage
    Nothing -> putStrLn usage
  where
    handler' webRoot port _ url req = runReaderT (handler url req) (ServerSettings webRoot port)
    usage = "./WebServer --root <web_root> [--port <port>] [--bind <address>]"
