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
import Control.Distributed.Process (Process,ProcessId,send,receiveWait,match)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)


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
      | (["tell"] == path) = tellApp req respond
      | otherwise          = liftIO $ staticApp req respond -- static html/js/css files
    path = Wai.pathInfo req


tellApp :: WaiApplication'
tellApp req respond = do
  xs <- liftIO $ T.unpack . decodeUtf8 <$> Wai.requestBody req
  case readMaybe xs :: Maybe Int of
    Nothing -> tellStringApp xs req respond
    Just n  -> tellIntApp n req respond


tellStringApp :: String -> WaiApplication'
tellStringApp xs req respond = do
  tellString xs
  liftIO $ respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] "OK String"

tellIntApp :: Int -> WaiApplication'
tellIntApp n req respond = do
  tellInt n
  liftIO $ respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] "OK Int"


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
--    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["main.html"] -- default content

  
  

tellString :: String -> IO' ()
tellString msg = do
  (node,mpid) <- ask
  liftIO $ runProcess node $ send mpid msg

tellInt :: Int -> IO' ()
tellInt n = do
  (node,mpid) <- ask
  liftIO $ runProcess node $ send mpid n



forkMain :: LocalNode -> IO ProcessId
forkMain node = forkProcess node mainProcess

mainProcess :: Process ()    
mainProcess = forever $ receiveWait [match pString,match pInt]
  where
    pString :: String -> Process ()
    pString msg = liftIO $ putStrLn $ "String: " ++ msg
    pInt :: Int -> Process ()
    pInt n = liftIO $ putStrLn $ "Int: " ++ show n

