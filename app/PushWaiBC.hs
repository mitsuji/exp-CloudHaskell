{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import Data.IORef(newIORef,writeIORef,readIORef)
import Text.Read (readMaybe)

import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Data.Word8 (_question)
import Data.List (delete)

import Control.Monad.Reader



main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  spid <- forkProcess node $ pushServer []
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (wsRouterApp node spid) (httpRouterApp node spid)


type IO' = ReaderT (LocalNode,ProcessId) IO
type WaiApplication' = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO' Wai.ResponseReceived
type WSServerApp' = WS.PendingConnection -> IO' ()


askNode :: IO' LocalNode
askNode = fst <$> ask

askPid :: IO' ProcessId
askPid = snd <$> ask


httpRouterApp :: LocalNode -> ProcessId -> Wai.Application
httpRouterApp node spid req respond = runReaderT (f req respond) (node,spid)
  where
    f req respond
      | (["push"] == path) = pushApp req respond
      | otherwise          = lift $ staticApp req respond -- static html/js/css files
    path = Wai.pathInfo req


pushApp :: WaiApplication'
pushApp req respond = do
  case cmd req of
    Just xs -> do
      case readMaybe xs :: Maybe Int of
        Nothing -> sendString xs req respond
        Just n -> sendInt n req respond
    Nothing -> lift $ respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] "[noParam]"
  where
    cmd :: Wai.Request -> Maybe String
    cmd req = lookup "cmd" (Wai.queryString req) >>= (\mcmd -> T.unpack . decodeUtf8 <$> mcmd)


sendString :: String -> WaiApplication'
sendString xs req respond = do
  msb <- pushString xs
  lift $ do
    case msb of
      Just sb -> do
        let pt_lbs = LBS.fromStrict $ encodeUtf8 $ T.pack $ "result: " ++ xs ++ " -> " ++ sb
        respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] pt_lbs
      Nothing -> do
        respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] "[noResponse]"


sendInt :: Int -> WaiApplication'
sendInt n req respond = do
  msb <- pushInt n
  lift $ do
    case msb of
      Just sb -> do
        let pt_lbs = LBS.fromStrict $ encodeUtf8 $ T.pack $ "result: " ++ (show n) ++ " -> " ++ (show sb)
        respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] pt_lbs
      Nothing -> do
        respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] "[noResponse]"



staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
--    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["main.html"] -- default content


wsRouterApp :: LocalNode -> ProcessId -> WS.ServerApp
wsRouterApp node spid pconn = runReaderT (f pconn) (node,spid)
  where
    f pconn
      | ("/viewer" == path) = viewerApp pconn
      | otherwise = lift $ WS.rejectRequest pconn "endpoint not found"
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


viewerApp :: WSServerApp'
viewerApp pconn = do
  node <- askNode
  conn <- lift $ WS.acceptRequest pconn
  vpid <- lift $ forkProcess node $ viewerServer conn
  registViewer vpid
  loop conn vpid
  where
    loop :: WS.Connection -> ProcessId -> IO' ()
    loop conn vpid = do
      msg <- lift $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          unregistViewer vpid
          -- [TODO] kill process
          lift $ putStrLn "close complete!!"
        _ -> loop conn vpid


type State = [ProcessId]

data Push = Regist ProcessId
          | Unregist ProcessId
          | StringData ProcessId String
          | IntData ProcessId Int
          deriving (Generic,Typeable)

instance Binary Push


pushServer :: State -> Process ()    
pushServer guests = do
  guests' <- receiveWait [match (processPush guests)]
  pushServer guests'


processPush :: State -> Push -> Process State
processPush guests (Regist sender) = do
  return $ sender : guests
processPush guests (Unregist sender) = do
  return $ delete sender guests
processPush guests (StringData sender msg) = do
  let r = "*" ++ msg ++ "*"
  send sender r
  mapM_ (\g-> send g r) guests
  liftIO $ do
    putStrLn $ "string: " ++ msg
  return $ guests
processPush guests (IntData sender n) = do
  let r = n * 2 + 1
  send sender r
  mapM_ (\g-> send g r) guests
  liftIO $ do
    putStrLn $ "int: " ++ show n
  return $ guests



viewerServer :: WS.Connection -> Process ()    
viewerServer conn = do
  receiveWait [match (notifyString conn), match (notifyInt conn)]
  viewerServer conn

notifyString :: WS.Connection -> String -> Process ()
notifyString conn xs =
  let
    txt = T.pack $ "notify: " ++ xs
  in
   liftIO $ WS.sendTextData conn txt
   
notifyInt :: WS.Connection -> Int -> Process ()
notifyInt conn n =
  let
    txt =T.pack $ "notify: " ++ show n
  in
   liftIO $ WS.sendTextData conn txt




registViewer :: ProcessId -> IO' ()
registViewer vpid = do
  node <- askNode
  spid <- askPid
  lift $ runProcess node $ send spid (Regist vpid)

unregistViewer :: ProcessId -> IO' ()
unregistViewer vpid = do
  node <- askNode
  spid <- askPid
  lift $ runProcess node $ send spid (Unregist vpid)

pushString :: String -> IO' (Maybe String)
pushString xs = do
  node <- askNode
  spid <- askPid
  lift $ do
    rsb <- newIORef Nothing
    runProcess node $ (\rsb -> do 
                          self <- getSelfPid
                          send spid (StringData self xs)
                          sb <- expectTimeout 1000000 -- must have timeout
                          liftIO $ writeIORef rsb sb
                      ) rsb
    sb <- readIORef rsb
    return sb

pushInt :: Int -> IO' (Maybe Int)
pushInt n = do
  node <- askNode
  spid <- askPid
  lift $ do
    rsb <- newIORef Nothing
    runProcess node $ (\rsb -> do 
                          self <- getSelfPid
                          send spid (IntData self n)
                          sb <- expectTimeout 1000000 -- must have timeout
                          liftIO $ writeIORef rsb sb
                      ) rsb
    sb <- readIORef rsb
    return sb

