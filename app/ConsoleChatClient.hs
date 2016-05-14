
import System.Environment (getArgs)
import System.IO (hSetEcho,stdin)
import Data.String (fromString)
import Text.Read (readMaybe)
import Network.Transport (EndPointAddress(EndPointAddress))
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess,runProcess)
import Control.Distributed.Process (Process,ProcessId,nsendRemote,receiveWait,match,NodeId(NodeId))
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import ConsoleChatData (MainMsg(RegistClient,UnregistClient,StringData,IntData))



type IO' = ReaderT (LocalNode,NodeId) IO

main :: IO ()
main = do
  host:port:mep:_ <- getArgs
  
  Right t <- createTransport host port defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  pid <- forkClient node
  let nid = NodeId $ EndPointAddress $ fromString mep

  hSetEcho stdin False
  runReaderT (client pid) (node,nid)
    where
      client :: ProcessId -> IO' ()
      client pid = do
        registClient pid
        loop pid

      loop :: ProcessId -> IO' ()
      loop pid = do
        msg <- liftIO getLine
        case readMaybe msg :: Maybe Int of
          Nothing | msg == "q" -> do
            unregistClient pid
            return ()
          Nothing -> tellString msg >> loop pid
          Just n  -> tellInt n >> loop pid


registClient :: ProcessId -> IO' ()
registClient pid = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (RegistClient pid)
  -- [TODO] link or monitor nid

unregistClient :: ProcessId -> IO' ()
unregistClient pid = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (UnregistClient pid)
  -- [TODO] kill pid

tellString :: String -> IO' ()
tellString msg = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (StringData msg)

tellInt :: Int -> IO' ()
tellInt n = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (IntData n)



forkClient :: LocalNode -> IO ProcessId
forkClient node = forkProcess node $ clientProcess

clientProcess :: Process ()    
clientProcess = forever $ receiveWait [match pString, match pInt]
  where
    pString :: String -> Process ()
    pString msg = liftIO $ putStrLn $ "notify: " ++ msg
    pInt :: Int -> Process ()
    pInt n = liftIO $ putStrLn $ "notify: " ++ show n

