-- modified version of: https://github.com/himura/twitter-conduit/blob/master/sample/userstream.hs

{-# LANGUAGE OverloadedStrings #-}

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

import Control.Monad
import Control.Monad.IO.Class

import Data.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as S8
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
  oauthKey <- readFile' "private/oauth.consumer"
  oauthSecret <- readFile' "private/oauth.consumer.secret"
  oauthAccess <- readFile' "private/oauth.access"
  oauthAccessSecret <- readFile' "private/oauth.access.secret"

  let oauth = twitterOAuth { oauthConsumerKey = oauthKey,
                             oauthConsumerSecret = oauthSecret }
      cred = Credential [ ("oath_token", oauthAccess),
                          ("oath_token_secret", oauthAccessSecret) ]
      in
   return (oauth, cred)
    where
      strip = filter (\x -> not $ x `elem` [' ','\t','\r','\n'])
      readFile' = (S8.pack <$>) . (liftM strip) . readFile

getTwitterInfo :: IO TWInfo
getTwitterInfo = do
  (oauth, cred) <- getOAuthTokens
  return $ (setCredential oauth cred def) { twProxy = Nothing }
  
main :: IO ()
main = do
  twitterInfo <- getTwitterInfo
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    src <- stream twitterInfo manager $ statusesFilterByTrack "cats"
    --src <- stream twitterInfo manager userstream
    src C.$$+- CL.mapM_ (liftIO . printStatus)

printStatus :: StreamingAPI -> IO ()
printStatus (SStatus s) = print "hi"
printStatus (SRetweetedStatus s) = print "hello"
printStatus _ = print "other"
