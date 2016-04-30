import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Text.Read (readMaybe)

type State = Int

processString :: State -> (ProcessId, String) -> Process State
processString count (sender,msg) = do
  send sender $ "*" ++ msg ++ "*"
  liftIO $ do
    putStrLn $ "string: " ++ msg
    putStrLn $ "count: " ++ show count
  return $ count+1

processInt :: State -> (ProcessId, Int) -> Process State
processInt count (sender,n) = do
  send sender $ n * 2 + 1
  liftIO $ do
    putStrLn $ "int: " ++ show n
    putStrLn $ "count: " ++ show count
  return $ count+1

main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  spid <- forkProcess node $ serverLoop 0
          
  clientLoop node spid
  where
    serverLoop state = do
      state' <- receiveWait [match (processString state), match (processInt state)]
      serverLoop state'
      
    clientLoop node tpid = do
      xs <- getLine
      runProcess node $ do
        self <- getSelfPid
        case readMaybe xs :: Maybe Int of
          Nothing -> sendString tpid self xs
          Just num -> sendInt tpid self num
      clientLoop node tpid
      
    
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
  
