{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module ConsoleChatData (
  MainMsg (RegistClient,UnregistClient,StringData)
  ) where


import Control.Distributed.Process (ProcessId)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

data MainMsg = RegistClient ProcessId String
             | UnregistClient ProcessId
             | StringData ProcessId String
             deriving (Generic,Typeable)

instance Binary MainMsg

