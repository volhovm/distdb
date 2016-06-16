{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic types common for the whole system
module Types
       ( Key
       , Value
       , Entry (..)
       , Host (..)
       , Port (..)
       , NetworkConfig (..)
       , Request (..)
       , Response (..)
       , responseMatches
       ) where

import qualified Data.Map as M

type Key = String
type Value = String

data Entry = Entry
    { eKey   :: Key
    , eValue :: Value
    } deriving (Eq,Ord)

instance Show Entry where
    show (Entry k v) = k ++ " " ++ v

instance Read Entry where
    readsPrec _ input = let (k:v:s) = words input in [(Entry k v, concat s)]

newtype Host = Host { getHost :: String }
               deriving (Show)

newtype Port = Port { getPort :: Word }
               deriving (Show, Num)

data NetworkConfig =
    NetworkConfig { networkSize :: Int
                  , portMap     :: M.Map Int (Host,Port)
                  , timeout     :: Int
                  }

data Request
    = GetEntry Key
    | SetEntry Entry
    | DeleteEntry Key
    | Ping
    deriving (Show,Read,Eq)

data Response
    = EntryFound Entry
    | EntrySet
    | EntryDeleted
    | EntryNotFound
    | Pong
    deriving (Show,Read,Eq)

responseMatches :: Request -> Response -> Bool
responseMatches rq rs =
    case (rq, rs) of
        (GetEntry _,EntryFound _)     -> True
        (GetEntry _,EntryNotFound)    -> True
        (SetEntry _,EntrySet)         -> True
        (DeleteEntry _,EntryDeleted)  -> True
        (DeleteEntry _,EntryNotFound) -> True
        (Ping,Pong)                   -> True
        _                             -> False
