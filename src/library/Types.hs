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
       , NetworkConfig (..)
       , Pinging (..)
       , EntryRequest (..)
       , EntryResponse (..)
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

data NetworkConfig =
    NetworkConfig { networkSize :: Int
                  , portMap     :: M.Map Int (Host,Port)
                  , timeout     :: Int
                  }

data Pinging
    = Ping
    | Pong
    deriving (Show,Read,Eq,Generic,Typeable)

data EntryRequest
    = GetEntry Key
    | SetEntry Entry
    | DeleteEntry Key
    deriving (Show,Read,Eq,Generic,Typeable)

data EntryResponse
    = EntryFound Entry
    | EntrySet
    | EntryDeleted
    | EntryNotFound
    deriving (Show,Read,Eq,Generic,Typeable)

instance Binary Entry
instance Binary Pinging
instance Binary EntryRequest
