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


httpRouterApp :: LocalNode -> ProcessId -> Wai.Application
httpRouterApp node spid req respond
  | (["push"] == path) = pushApp node spid req respond
  | otherwise          = staticApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req


pushApp :: LocalNode -> ProcessId -> Wai.Application
pushApp node spid req respond = do
  case cmd req of
    Just xs -> do
      case readMaybe xs :: Maybe Int of
        Nothing -> sendString node spid xs req respond
        Just n -> sendInt node spid n req respond
    Nothing -> respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] "[noParam]"
  where
    cmd :: Wai.Request -> Maybe String
    cmd req = lookup "cmd" (Wai.queryString req) >>= (\mcmd -> T.unpack . decodeUtf8 <$> mcmd)


sendString :: LocalNode -> ProcessId -> String  -> Wai.Application
sendString node spid xs req respond = do
  msb <- pushString node spid xs
  case msb of
    Just sb -> do
      let pt_lbs = LBS.fromStrict $ encodeUtf8 $ T.pack $ "result: " ++ xs ++ " -> " ++ sb
      respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] pt_lbs
    Nothing -> do
      respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] "[noResponse]"


sendInt :: LocalNode -> ProcessId -> Int -> Wai.Application
sendInt node spid n req respond = do
  msb <- pushInt node spid n
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
wsRouterApp node spid pconn
  | ("/viewer" == path) = viewerApp node spid pconn
  | otherwise = WS.rejectRequest pconn "endpoint not found"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


viewerApp :: LocalNode -> ProcessId -> WS.ServerApp
viewerApp node spid pconn = do
  conn <- WS.acceptRequest pconn
  vpid <- forkProcess node $ viewerServer conn
  registViewer node spid vpid
  loop conn vpid
  where
    loop conn vpid = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          unregistViewer node spid vpid
          -- [TODO] kill process
          putStrLn "close complete!!"
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




registViewer :: LocalNode -> ProcessId -> ProcessId -> IO ()
registViewer node spid vpid = runProcess node $ send spid (Regist vpid)

unregistViewer :: LocalNode -> ProcessId -> ProcessId -> IO ()
unregistViewer node spid vpid = runProcess node $ send spid (Unregist vpid)

pushString :: LocalNode -> ProcessId -> String -> IO (Maybe String)
pushString node spid xs = do
  rsb <- newIORef Nothing
  runProcess node $ (\rsb -> do 
                        self <- getSelfPid
                        send spid (StringData self xs)
                        sb <- expectTimeout 1000000 -- must have timeout
                        liftIO $ writeIORef rsb sb
                    ) rsb
  sb <- readIORef rsb
  return sb

pushInt :: LocalNode -> ProcessId -> Int -> IO (Maybe Int)
pushInt node spid n = do
  rsb <- newIORef Nothing
  runProcess node $ (\rsb -> do 
                        self <- getSelfPid
                        send spid (IntData self n)
                        sb <- expectTimeout 1000000 -- must have timeout
                        liftIO $ writeIORef rsb sb
                    ) rsb
  sb <- readIORef rsb
  return sb

