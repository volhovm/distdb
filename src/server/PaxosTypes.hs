{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Types used in multipaxos consensus algo
module PaxosTypes
       ( Slot
       , CommandId
       , Command (..)
       , ClientRequest
       , ReplicaState (..)
       , slotIn, slotOut, requests, proposals, decisions
       , emptyReplicaState
       , ProposeRequest (..)
       , Decision (..)
       ) where

import           Communication (Message, SendableLike)
import           Control.Lens  (makeLenses)
import           Data.Binary   (Binary (..))
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Types         (EntryRequest)

import qualified Data.Set      as S

type Slot = Int
type CommandId = Int -- ? Hash?

data Command = Command CommandId EntryRequest
               deriving (Show,Read,Eq,Ord,Generic)

type ClientRequest = Message Command

data ReplicaState = ReplicaState
    { _slotIn    :: Slot
    , _slotOut   :: Slot
    , _requests  :: S.Set ClientRequest
    , _proposals :: S.Set (Slot, ClientRequest)
    , _decisions :: S.Set (Slot, ClientRequest)
    } deriving (Show, Read)
makeLenses ''ReplicaState

emptyReplicaState :: ReplicaState
emptyReplicaState = ReplicaState 1 1 S.empty S.empty S.empty

data ProposeRequest = ProposeRequest Slot ClientRequest
                      deriving (Show,Read,Generic,Typeable)

data Decision = Decision Slot ClientRequest
                deriving (Show,Read,Generic,Typeable)

instance Binary Command
instance Binary ProposeRequest
instance Binary Decision

instance SendableLike Command
instance SendableLike ProposeRequest
instance SendableLike Decision
