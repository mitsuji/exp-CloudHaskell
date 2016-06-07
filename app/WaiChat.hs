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
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS

import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess,runProcess)
import Control.Distributed.Process (Process,ProcessId,send,receiveWait,match)
import Control.Monad.Trans (liftIO)
import Data.Map.Strict as Map

import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Data.Word8 (_question)
import Data.List (delete)

import Control.Monad.Reader(ReaderT,runReaderT,ask)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)


data MainMsg = RegistClient ProcessId String
             | UnregistClient ProcessId
             | StringData ProcessId String
             deriving (Generic,Typeable)

instance Binary MainMsg



main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  mpid <- forkMain node
  host:port:_ <- getArgs
  return ()
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (wsRouterApp node mpid) staticApp


type IO' = ReaderT (LocalNode,ProcessId) IO
type WaiApplication' = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO' Wai.ResponseReceived
type WSServerApp' = WS.PendingConnection -> IO' ()


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["WaiChat.htm"] -- default content


wsRouterApp :: LocalNode -> ProcessId -> WS.ServerApp
wsRouterApp node mpid pconn = runReaderT (f pconn) (node,mpid)
  where
    f pconn
      | ("/client" == path) = clientApp pconn
      | otherwise = liftIO $ WS.rejectRequest pconn "endpoint not found"
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


clientApp :: WSServerApp'
clientApp pconn = do
  conn <- liftIO $ WS.acceptRequest pconn
  cpid <- forkClient conn
  registClient cpid name
  loop conn cpid
  where
    loop :: WS.Connection -> ProcessId -> IO' ()
    loop conn cpid = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          unregistClient cpid
          -- [TODO] kill process
          liftIO $ putStrLn "close complete!!"
        WS.DataMessage (WS.Text lbs) -> do
          tellString cpid $ T.unpack $ WS.fromLazyByteString lbs
          loop conn cpid
        _ -> loop conn cpid
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = BS.drop 1 $ BS.dropWhile (/=_question) requestPath
    name = T.unpack $ decodeUtf8 $ H.urlDecode True query  




registClient :: ProcessId -> String -> IO' ()
registClient pid name = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ send mnid (RegistClient pid name)
  -- [TODO] link or monitor nid

unregistClient :: ProcessId -> IO' ()
unregistClient pid = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ send mnid (UnregistClient pid)
  -- [TODO] kill pid

tellString :: ProcessId -> String -> IO' ()
tellString pid msg = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ send mnid (StringData pid msg)


forkClient :: WS.Connection -> IO' ProcessId
forkClient conn = do
  (node,_) <- ask
  liftIO $ forkProcess node $ clientProcess conn

clientProcess :: WS.Connection -> Process ()    
clientProcess conn = do
  receiveWait [match (p conn)]
  clientProcess conn
  where
    p :: WS.Connection-> String -> Process ()
    p conn msg = 
      let
        txt = T.pack $ msg
      in
       liftIO $ WS.sendTextData conn txt




forkMain :: LocalNode -> IO ProcessId
forkMain node = forkProcess node $ mainProcess Map.empty


type MainState = Map.Map ProcessId String

mainProcess :: MainState -> Process ()    
mainProcess state = do
  state' <- receiveWait [match (p state)]
  mainProcess state'
    where
      p :: MainState -> MainMsg -> Process MainState
      p state (RegistClient sender name)   = return $ Map.insert sender name state
      p state (UnregistClient sender) = return $ Map.delete sender state
      p state (StringData sender msg) = do
        let sname = case Map.lookup sender state of
              Just name -> name
              Nothing   -> "[unknown]"
        liftIO $ putStrLn $ "StringData " ++ sname ++ " " ++ msg
        mapM_ (\(t,_)-> send t $ sname ++ ": " ++ msg) $ Map.toList state
        return $ state


