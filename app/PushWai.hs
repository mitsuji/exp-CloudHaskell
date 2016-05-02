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




main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  spid <- forkProcess node $ pushServer 0
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ routerApp node spid



routerApp :: LocalNode -> ProcessId -> Wai.Application
routerApp node spid req respond
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
--    cmd req = lookup "cmd" (Wai.queryString req) >>= id >>= (\cmd -> return $ T.unpack $ decodeUtf8 cmd)
--    cmd req = do
--      mcmd <- lookup "cmd" (Wai.queryString req)
--      cmd <- mcmd
--      return $ T.unpack $ decodeUtf8 cmd


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



    
type State = Int

data Push = StringData ProcessId String
          | IntData ProcessId Int
          deriving (Generic,Typeable)

instance Binary Push


pushServer :: State -> Process ()    
pushServer state = do
  state' <- receiveWait [match (processPush state)]
  pushServer state'


processPush :: State -> Push -> Process State
processPush count (StringData sender msg) = do
  send sender $ "*" ++ msg ++ "*"
  liftIO $ do
    putStrLn $ "string: " ++ msg
    putStrLn $ "count: " ++ show count
  return $ count+1
processPush count (IntData sender n) = do
  send sender $ n * 2 + 1
  liftIO $ do
    putStrLn $ "int: " ++ show n
    putStrLn $ "count: " ++ show count
  return $ count+1


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

