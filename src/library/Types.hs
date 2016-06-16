-- | Basic types common for the whole system
module Types (Entry (..)) where

data Entry = Entry
    { eKey   :: String
    , eValue :: String
    } deriving (Show, Eq, Ord)
