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

import Control.Monad.Reader(ReaderT,runReaderT,ask,lift)
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import Data.Monoid((<>))


main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  mpid <- forkMain node
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (wsRouterApp node mpid) (httpRouterApp node mpid)


type IO' = ReaderT (LocalNode,ProcessId) IO
type WaiApplication' = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO' Wai.ResponseReceived
type WSServerApp' = WS.PendingConnection -> IO' ()


askNode :: IO' LocalNode
askNode = fst <$> ask

askPid :: IO' ProcessId
askPid = snd <$> ask


httpRouterApp :: LocalNode -> ProcessId -> Wai.Application
httpRouterApp node mpid req respond = runReaderT (f req respond) (node,mpid)
  where
    f req respond
      | (["input"] == path) = inputApp req respond
      | otherwise           = lift $ staticApp req respond -- static html/js/css files
    path = Wai.pathInfo req


inputApp :: WaiApplication'
inputApp req respond =
  case cmd req of
    Just xs ->
      case readMaybe xs :: Maybe Int of
        Nothing -> sendString xs req respond
        Just n -> sendInt n req respond
    Nothing ->
      lift $ respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] "[noParam]"
  where
    cmd :: Wai.Request -> Maybe String
    cmd req = lookup "cmd" (Wai.queryString req) >>= (\mcmd -> T.unpack . decodeUtf8 <$> mcmd)


sendString :: String -> WaiApplication'
sendString xs req respond = do
  msb <- stringData xs
  lift $
    let
      res = case msb of
        Just sb -> "result: " <> BB.fromString xs <> " -> " <> BB.fromString sb
        Nothing -> "[noResponse]"
    in
     respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] res


sendInt :: Int -> WaiApplication'
sendInt n req respond = do
  msb <- intData n
  lift $
    let
      res = case msb of
        Just sb -> "result: " <> BB.fromString (show n) <> " -> " <> BB.fromString (show sb)
        Nothing -> "[noResponse]"
    in
     respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] res



staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
--    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["main.html"] -- default content


wsRouterApp :: LocalNode -> ProcessId -> WS.ServerApp
wsRouterApp node mpid pconn = runReaderT (f pconn) (node,mpid)
  where
    f pconn
      | ("/viewer" == path) = viewerApp pconn
      | otherwise = lift $ WS.rejectRequest pconn "endpoint not found"
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


viewerApp :: WSServerApp'
viewerApp pconn = do
  conn <- lift $ WS.acceptRequest pconn
  vpid <- forkViewer conn
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





forkMain :: LocalNode -> IO ProcessId
forkMain node = forkProcess node $ mainProcess []

forkViewer :: WS.Connection -> IO' ProcessId
forkViewer conn = do
  node <- askNode
  lift $ forkProcess node $ viewerProcess conn

registViewer :: ProcessId -> IO' ()
registViewer vpid = do
  node <- askNode
  mpid <- askPid
  lift $ runProcess node $ send mpid (RegistViewer vpid)

unregistViewer :: ProcessId -> IO' ()
unregistViewer vpid = do
  node <- askNode
  mpid <- askPid
  lift $ runProcess node $ send mpid (UnregistViewer vpid)

stringData :: String -> IO' (Maybe String)
stringData xs = do
  node <- askNode
  mpid <- askPid
  lift $ do
    rsb <- newIORef Nothing
    runProcess node $ (\rsb -> do 
                          self <- getSelfPid
                          send mpid (StringData self xs)
                          sb <- expectTimeout 1000000 -- must have timeout
                          liftIO $ writeIORef rsb sb
                      ) rsb
    sb <- readIORef rsb
    return sb

intData :: Int -> IO' (Maybe Int)
intData n = do
  node <- askNode
  mpid <- askPid
  lift $ do
    rsb <- newIORef Nothing
    runProcess node $ (\rsb -> do 
                          self <- getSelfPid
                          send mpid (IntData self n)
                          sb <- expectTimeout 1000000 -- must have timeout
                          liftIO $ writeIORef rsb sb
                      ) rsb
    sb <- readIORef rsb
    return sb




type MainState = [ProcessId]

data MainMsg = RegistViewer ProcessId
             | UnregistViewer ProcessId
             | StringData ProcessId String
             | IntData ProcessId Int
             deriving (Generic,Typeable)

instance Binary MainMsg


mainProcess :: MainState -> Process ()    
mainProcess state = do
  state' <- receiveWait [match (p state)]
  mainProcess state'
  where
    p :: MainState -> MainMsg -> Process MainState
    p state (RegistViewer sender)   = return $ sender : state
    p state (UnregistViewer sender) = return $ delete sender state
    p state (StringData sender msg) = do
      let r = "*" ++ msg ++ "*"
      send sender r
      mapM_ (\g-> send g r) state
      liftIO $ putStrLn $ "string: " ++ msg
      return $ state
    p state (IntData sender n) = do
      let r = n * 2 + 1
      send sender r
      mapM_ (\g-> send g r) state
      liftIO $ putStrLn $ "int: " ++ show n
      return $ state


viewerProcess :: WS.Connection -> Process ()    
viewerProcess conn = do
  receiveWait [match (pString conn), match (pInt conn)]
  viewerProcess conn
  where
    pString :: WS.Connection -> String -> Process ()
    pString conn xs =
      let
        txt = T.pack $ "notify: " ++ xs
      in
       liftIO $ WS.sendTextData conn txt
   
    pInt :: WS.Connection -> Int -> Process ()
    pInt conn n =
      let
        txt =T.pack $ "notify: " ++ show n
      in
       liftIO $ WS.sendTextData conn txt




