{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Instances where

import           Control.Distributed.Process.Internal.Types (LocalProcessId (..),
                                                             NodeId (..), ProcessId (..))
import           Network.Transport                          (EndPointAddress (..))


deriving instance Read EndPointAddress
deriving instance Read LocalProcessId
deriving instance Read NodeId
deriving instance Read ProcessId
