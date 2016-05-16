
import System.Environment (getArgs)
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess)
import Control.Distributed.Process (processNodeId,nodeAddress,Process,ProcessId,getSelfPid,register,send,receiveWait,match)
import Control.Monad.Trans (liftIO)
import ConsoleChatData (MainMsg(RegistClient,UnregistClient,StringData))
import Data.Map.Strict as Map


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
forkMain node = forkProcess node $ mainProcess Map.empty


type MainState = Map.Map ProcessId String

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
    p state (RegistClient sender name)   = return $ Map.insert sender name state
    p state (UnregistClient sender) = return $ Map.delete sender state
    p state (StringData sender msg) = do
      let sname = case Map.lookup sender state of
            Just name -> name
            Nothing   -> "[unknown]"
      liftIO $ putStrLn $ "StringData " ++ sname ++ " " ++ msg
      mapM_ (\(t,_)-> send t $ sname ++ ": " ++ msg) $ Map.toList state
      return $ state


