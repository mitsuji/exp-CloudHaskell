{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

type State = Int

data Push = StringData ProcessId String
          | IntData ProcessId Int
          deriving (Generic,Typeable)

instance Binary Push


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


main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  spid <- forkProcess node $ serverLoop 0
          
  clientLoop node spid
  where
    serverLoop state = do
      state' <- receiveWait [match (processPush state)]
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
    send tpid (StringData self xs)
--    msb <- expectTimeout 1000000 :: Process (Maybe String)
--    case msb of
--      Just sb -> liftIO $ putStrLn $ "result: " ++ sb
--      Nothing -> return ()


sendInt tpid self num = do
    send tpid (IntData self num)
--    msb <- expectTimeout 1000000 :: Process (Maybe Int)
--    case msb of
--      Just sb -> liftIO $ putStrLn $ "result: " ++ show sb
--      Nothing -> return ()
  
