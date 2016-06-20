-- | Types used in multipaxos consensus algo
module PaxosTypes
       ( Slot
       , ClientRequest
       , ReplicaState (..)
       , emptyReplicaState
       ) where

import           Communication (Message)
import           Types         (EntryRequest)

import qualified Data.Map      as M
import qualified Data.Set      as S

type Slot = Int

type ClientRequest = Message EntryRequest

data ReplicaState = ReplicaState
    { slotIn    :: Slot
    , slotOut   :: Slot
    , requests  :: S.Set ClientRequest
    , proposals :: M.Map Slot ClientRequest
    , decisions :: M.Map Slot ClientRequest
    } deriving (Show, Read)

emptyReplicaState :: ReplicaState
emptyReplicaState = ReplicaState 1 1 S.empty M.empty M.empty
