
import System.Environment (getArgs)
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess)
import Control.Distributed.Process (processNodeId,nodeAddress,Process,ProcessId,getSelfPid,register,send,receiveWait,match)
import Control.Monad.Trans (liftIO)
import ConsoleChatData (MainMsg(RegistClient,UnregistClient,StringData,IntData))
import Data.List (delete)



main :: IO ()
main = do
  host:port:_ <- getArgs
  Right t <- createTransport host port defaultTCPParameters
  pid <- newLocalNode t initRemoteTable >>= forkMain
  putStrLn $ "EndPoint: "  ++ (show $ nodeAddress $ processNodeId pid)
  _ <- getLine
  -- [TODO] kill pid
  return ()



forkMain :: LocalNode -> IO ProcessId
forkMain node = forkProcess node $ mainProcess []



type MainState = [ProcessId]

mainProcess :: MainState -> Process ()    
mainProcess state = do
  pid <- getSelfPid
  register "main" pid
  loop state
  where
    loop :: MainState -> Process ()
    loop state = do
      state' <- receiveWait [match (p state)]
      loop state'
    p :: MainState -> MainMsg -> Process MainState
    p state (RegistClient sender)   = return $ sender : state
    p state (UnregistClient sender) = return $ delete sender state
    p state (StringData msg) = do
      liftIO $ putStrLn $ "String: " ++ msg
      mapM_ (\g-> send g $ "*" ++ msg ++ "*") state
      return $ state
    p state (IntData n) = do
      liftIO $ putStrLn $ "Int: " ++ show n
      mapM_ (\g-> send g $ n * 2 + 1) state
      return $ state


