{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | Basic types common for the whole system

module Types
       ( Key
       , Value
       , Entry (..)
       , Host (..)
       , Port (..)
       , Role (..)
       , NetworkConfig (..)
       , Pinging (..)
       , EntryRequest (..)
       , EntryResponse (..)
       , responseMatches
       , Command (..)
       , CommandId
       ) where

import           Data.Binary   (Binary (..))
import qualified Data.Map      as M
import           Data.String   (IsString)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

type Key = String
type Value = String

data Entry = Entry
    { eKey   :: Key
    , eValue :: Value
    } deriving (Eq,Ord,Generic,Typeable)

instance Show Entry where
    show (Entry k v) = k ++ " " ++ v

instance Read Entry where
    readsPrec _ input = let (k:v:s) = words input in [(Entry k v, concat s)]

newtype Host = Host { getHost :: String }
               deriving (Show, Read, IsString)

newtype Port = Port { getPort :: Word }
               deriving (Show, Num)

data Role = Replica | Acceptor | Leader deriving (Show,Eq)

data NetworkConfig =
    NetworkConfig { portMapReplicas  :: M.Map Int (Host,Port)
                  , portMapAcceptors :: M.Map Int (Host,Port)
                  , portMapLeaders   :: M.Map Int (Host,Port)
                  , portMap          :: M.Map Int (Host,Port,[Role])
                  , timeout          :: Int
                  }

data Pinging
    = Ping
    | Pong
    deriving (Show,Read,Eq,Ord,Generic,Typeable)

data EntryRequest
    = GetEntry Key
    | SetEntry Entry
    | DeleteEntry Key
    deriving (Show,Read,Eq,Ord,Generic,Typeable)

data EntryResponse
    = EntryFound Entry
    | EntrySet
    | EntryDeleted
    | EntryNotFound
    deriving (Show,Read,Eq,Ord,Generic,Typeable)

instance Binary Entry
instance Binary Pinging
instance Binary EntryRequest
instance Binary EntryResponse

responseMatches :: EntryRequest -> EntryResponse -> Bool
responseMatches rq rs =
    case (rq, rs) of
        (GetEntry _,EntryFound _)     -> True
        (GetEntry _,EntryNotFound)    -> True
        (SetEntry _,EntrySet)         -> True
        (DeleteEntry _,EntryDeleted)  -> True
        (DeleteEntry _,EntryNotFound) -> True
        _                             -> False


data ConsensusRequest = ConsensusRequest EntryRequest
                        deriving (Typeable,Generic)
instance Binary ConsensusRequest

type CommandId = Int -- ? Hash?
data Command = Command CommandId EntryRequest
               deriving (Show,Read,Eq,Ord,Generic)

instance Binary Command
