{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module ConsoleChatData (
  MainMsg (RegistClient,UnregistClient,StringData,IntData)
  ) where


import Control.Distributed.Process (ProcessId)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

data MainMsg = RegistClient ProcessId
             | UnregistClient ProcessId
             | StringData String
             | IntData Int
             deriving (Generic,Typeable)

instance Binary MainMsg

