{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
module Communication
       ( Message (..)
       , Sendable (..)
       , SendableLike (..)
       , PolyMessage (..)
       ) where

import           Control.Distributed.Process (Process, ProcessId, getSelfPid, send)
import           Data.Binary                 (Binary (..))
import           Data.Typeable               (Typeable)
import           GHC.Generics                (Generic)


import           Instances                   ()
import           Types                       (Command (..), EntryRequest (..),
                                              EntryResponse (..), Pinging (..))

-- | Explicitely typed message
data Message a = Message
    { msgFrom :: ProcessId
    , msgBody :: a
    } deriving (Show,Generic,Typeable,Read,Eq,Ord)
instance (Binary a) => Binary (Message a)

-- | Class for send-over-network datatypes to put them in heterohenous
-- collection
class (Binary a, Show a, Typeable a) => SendableLike a where
    send' :: ProcessId -> a -> Process ()
    send' pid x = do
        self <- getSelfPid
        send pid $ Message self x

instance SendableLike Pinging
instance SendableLike EntryRequest
instance SendableLike EntryResponse
instance SendableLike Command

-- | Heterogehuous datatype wrapper
data Sendable = forall a . SendableLike a => Sendable { unSendable :: a }

instance Show Sendable where
    show (Sendable a) = "Sendable wrapper of: { " ++ show a ++ " }"

-- | Polymorphic message with something sendable inside
data PolyMessage = PolyMessage
    { msgTo'   :: ProcessId
    , msgBody' :: Sendable
    } deriving (Show)
