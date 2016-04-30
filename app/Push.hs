import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Text.Read (readMaybe)

processString :: (ProcessId, String) -> Process ()
processString (sender,msg) = do
  liftIO $ putStrLn $ "string: " ++ msg
  send sender $ "*" ++ msg ++ "*"

processInt :: (ProcessId, Int) -> Process ()
processInt (sender,n) = do
  liftIO $ putStrLn $ "int: " ++ show n
  send sender $ n * 2 + 1

main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  spid <- forkProcess node $ forever $
          receiveWait [match processString, match processInt]
  loop node spid
  where
    loop node tpid = do
      xs <- getLine
      runProcess node $ do
        self <- getSelfPid
        case readMaybe xs :: Maybe Int of
          Nothing -> sendString tpid self xs
          Just num -> sendInt tpid self num
      loop node tpid
    
sendString tpid self xs = do
    send tpid (self,xs)
    msb <- expectTimeout 1000000 :: Process (Maybe String)
    case msb of
      Just sb -> liftIO $ putStrLn $ "result: " ++ sb
      Nothing -> return ()
  
sendInt tpid self num = do
    send tpid (self,num)
    msb <- expectTimeout 1000000 :: Process (Maybe Int)
    case msb of
      Just sb -> liftIO $ putStrLn $ "result: " ++ show sb
      Nothing -> return ()
  
