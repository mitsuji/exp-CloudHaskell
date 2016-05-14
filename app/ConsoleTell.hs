
import System.IO (hSetEcho,stdin)
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
  mpid <- forkMain node

  hSetEcho stdin False
  runReaderT clientLoop (node,mpid)
  where
    clientLoop :: IO' ()
    clientLoop = do
      msg <- liftIO getLine
      case readMaybe msg :: Maybe Int of
        Nothing -> tellString msg
        Just n -> tellInt n
      clientLoop



type IO' = ReaderT (LocalNode,ProcessId) IO

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

