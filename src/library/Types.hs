{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic types common for the whole system
module Types
       ( Entry (..)
       , Host (..)
       , Port (..)
       , NetworkConfig (..)
       , Request (..)
       , Response (..)
       , responseMatches
       ) where

import qualified Data.Map as M

data Entry = Entry
    { eKey   :: String
    , eValue :: String
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
    = GetEntry String
    | SetEntry Entry
    | DeleteEntry String
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
