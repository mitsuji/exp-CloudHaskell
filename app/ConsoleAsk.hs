
import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess,runProcess)
import Control.Distributed.Process (Process,ProcessId,send,receiveWait,match,getSelfPid,expectTimeout)
import Control.Monad (forever)
import Control.Monad.Trans (lift,liftIO)
import Control.Monad.Reader (ReaderT,runReaderT,ask)
import Text.Read (readMaybe)
import System.IO (hSetEcho,stdin)
import Data.IORef(newIORef,writeIORef,readIORef)



main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  mpid <- forkMain node

  hSetEcho stdin False
  runReaderT clientLoop (node,mpid)
  where
    clientLoop :: IO' ()
    clientLoop = do
      msg<- liftIO getLine
      case readMaybe msg :: Maybe Int of
        Nothing -> do
          mmsg' <- askString msg
          case mmsg' of
            Just msg' -> liftIO $ putStrLn $ "String: " ++ msg ++ " Result: " ++ msg'
            Nothing   -> liftIO $ putStrLn $ "String: " ++ msg ++ " NoResult"
        Just n -> do
          mn' <- askInt n
          case mn' of
            Just n' -> liftIO $ putStrLn $ "Int: " ++ show n ++ " Result: " ++ show n'
            Nothing -> liftIO $ putStrLn $ "Int: " ++ show n ++ " NoResult"
      clientLoop



type IO' = ReaderT (LocalNode,ProcessId) IO

askString :: String -> IO' (Maybe String)
askString msg = do
  (node,mpid) <- ask
  lift $ do
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
  lift $ do
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

