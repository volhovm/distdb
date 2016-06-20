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
       , Decision (..)
       , Ballot
       , PValue
       , AcceptorState (..)
       , aBallotNum, accepted
       , emptyAcceptorState
       , PhaseCommitA (..)
       , PhaseCommitB (..)
       , LeaderNotification (..)
       , LeaderState (..)
       ,
       ) where

import           Control.Lens  (makeLenses)
import           Data.Binary   (Binary (..))
import qualified Data.Set      as S
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import           Communication (Message, SendableLike)
import           Types         (EntryRequest)

type Slot = Int
type CommandId = Int -- ? Hash?
type Ballot = Int
type LeaderId = Int
type PValue = (Ballot, Slot, Command)
type ClientRequest = Message Command

data Command = Command CommandId EntryRequest
               deriving (Show,Read,Eq,Ord,Generic)


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


data Decision = Decision Slot ClientRequest
                deriving (Show,Read,Generic,Typeable)

instance Binary Command
instance Binary Decision
instance SendableLike Command
instance SendableLike Decision


data AcceptorState = AcceptorState
    { _aBallotNum :: Ballot
    , _accepted   :: S.Set PValue
    } deriving (Show,Read)
makeLenses ''AcceptorState

emptyAcceptorState :: AcceptorState
emptyAcceptorState = AcceptorState (-1) S.empty

data PhaseCommitA
    = P1A LeaderId Ballot
    | P2A LeaderId PValue
    deriving (Show,Read,Generic,Typeable)

data PhaseCommitB
    = P1B LeaderId Ballot (S.Set PValue)
    | P2B (Ballot,LeaderId)
    deriving (Show,Read,Generic,Typeable)

instance Binary PhaseCommitA
instance Binary PhaseCommitB
instance SendableLike PhaseCommitA
instance SendableLike PhaseCommitB


data LeaderState = LeaderState
    { _lBallotNum :: Ballot
    , _lActive    :: Bool
    , _lProposals :: S.Set (Slot, ClientRequest)
    } deriving (Show,Read)
makeLenses ''LeaderState

-- What leader gets
data LeaderNotification =
    ProposeRequest Slot ClientRequest
    | Adopted Ballot [PValue]
    | Preempted Ballot
    deriving (Show,Read,Generic,Typeable)

instance Binary LeaderNotification
instance SendableLike LeaderNotification
