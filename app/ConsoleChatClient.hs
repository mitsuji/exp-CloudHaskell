
import System.Environment (getArgs)
import System.IO (hSetEcho,stdin)
import Data.String (fromString)
import Network.Transport (EndPointAddress(EndPointAddress))
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess,runProcess)
import Control.Distributed.Process (Process,ProcessId,nsendRemote,receiveWait,match,NodeId(NodeId))
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import ConsoleChatData (MainMsg(RegistClient,UnregistClient,StringData))



type IO' = ReaderT (LocalNode,NodeId) IO

main :: IO ()
main = do
  host:port:mep:name:_ <- getArgs
  
  Right t <- createTransport host port defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  pid <- forkClient node
  let nid = NodeId $ EndPointAddress $ fromString mep

  hSetEcho stdin False
  runReaderT (client pid name) (node,nid)
    where
      client :: ProcessId -> String -> IO' ()
      client pid name = do
        registClient pid name
        loop pid

      loop :: ProcessId -> IO' ()
      loop pid = do
        msg <- liftIO getLine
        case msg of
          "q" -> unregistClient pid
          _   -> tellString pid msg >> loop pid


registClient :: ProcessId -> String -> IO' ()
registClient pid name = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (RegistClient pid name)
  -- [TODO] link or monitor nid

unregistClient :: ProcessId -> IO' ()
unregistClient pid = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (UnregistClient pid)
  -- [TODO] kill pid

tellString :: ProcessId -> String -> IO' ()
tellString pid msg = do
  (node,mnid) <- ask
  liftIO $ runProcess node $ nsendRemote mnid "main" (StringData pid msg)


forkClient :: LocalNode -> IO ProcessId
forkClient node = forkProcess node $ clientProcess

clientProcess :: Process ()    
clientProcess = forever $ receiveWait [match p]
  where
    p :: String -> Process ()
    p msg = liftIO $ putStrLn $ msg

