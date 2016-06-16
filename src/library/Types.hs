-- | Basic types common for the whole system
module Types
       ( Entry (..)
       , Request (..)
       , Response (..)
       , responseMatches
       ) where

data Entry = Entry
    { eKey   :: String
    , eValue :: String
    } deriving (Eq,Ord)

instance Show Entry where
    show (Entry k v) = show k ++ " " ++ show v

instance Read Entry where
    readsPrec _ input = let (k:v:s) = words input in [(Entry k v, concat s)]

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
