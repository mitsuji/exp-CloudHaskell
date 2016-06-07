{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.String (fromString)
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Application.Static as Static
import Data.Maybe (fromJust)
import Data.FileEmbed (embedDir)
import WaiAppStatic.Types (toPieces)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Text.Read (readMaybe)
import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess,runProcess)
import Control.Distributed.Process (Process,ProcessId,send,receiveWait,match,getSelfPid,expectTimeout)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import Data.IORef(newIORef,writeIORef,readIORef)


main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  spid <- forkMain node
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ routerApp node spid



type IO' = ReaderT (LocalNode,ProcessId) IO
type WaiApplication' = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO' Wai.ResponseReceived


routerApp :: LocalNode -> ProcessId -> Wai.Application
routerApp node mpid req respond = runReaderT (f req respond) (node,mpid)
  where
    f req respond
      | (["ask"] == path) = askApp req respond
      | otherwise          = liftIO $ staticApp req respond -- static html/js/css files
    path = Wai.pathInfo req


askApp :: WaiApplication'
askApp req respond = do
  xs <- liftIO $ T.unpack . decodeUtf8 <$> Wai.requestBody req
  case readMaybe xs :: Maybe Int of
    Nothing -> askStringApp xs req respond
    Just n  -> askIntApp n req respond


askStringApp :: String -> WaiApplication'
askStringApp xs req respond = do
  mmsg <- askString xs
  let r = case mmsg of
        Just msg ->  fromString msg
        Nothing  ->  "[NoResponse]"
  liftIO $ respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] r

askIntApp :: Int -> WaiApplication'
askIntApp n req respond = do
  mn <- askInt n
  let r = case mn of
        Just n  -> fromString (show n)
        Nothing -> "[NoResponse]"
  liftIO $ respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] r
      
staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["WaiAsk.htm"] -- default content

  
  

askString :: String -> IO' (Maybe String)
askString msg = do
  (node,mpid) <- ask
  liftIO $ do
    rr <- newIORef Nothing
    runProcess node $ do 
      self <- getSelfPid
      send mpid (self,msg)
      r <- expectTimeout 1000000 -- must have timeout
      liftIO $ writeIORef rr r
    r <- readIORef rr
    return r
    

askInt :: Int -> IO' (Maybe Int)
askInt n = do
  (node,mpid) <- ask
  liftIO $ do
    rr <- newIORef Nothing
    runProcess node $ do 
      self <- getSelfPid
      send mpid (self,n)
      r <- expectTimeout 1000000 -- must have timeout
      liftIO $ writeIORef rr r
    r <- readIORef rr
    return r



forkMain :: LocalNode -> IO ProcessId
forkMain node = forkProcess node mainProcess

mainProcess :: Process ()    
mainProcess = forever $ receiveWait [match pString,match pInt]
  where
    pString :: (ProcessId,String) -> Process ()
    pString (sender,msg) = send sender $ "*" ++ msg ++ "*"
    pInt :: (ProcessId,Int) -> Process ()
    pInt (sender,n) = send sender $ n * 2 + 1
