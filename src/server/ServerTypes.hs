{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Basic types for server, server state/monad

module ServerTypes
       ( ServerConfig (..)

       , WriterPart (..)
       , writeMsg, writeLog

       , ServerState (..)
       , hashmap, replica, acceptor, leader
       , emptyServerState
       , dumpServerState
       , readServerState

       , ServerM (..)
       , writeMsg'
       ) where

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process (ProcessId)
import           Control.Exception           (SomeException (..), catch)
import           Control.Lens                (makeLenses)
import           Control.Monad.IO.Class      (MonadIO (..), liftIO)
import           Control.Monad.RWS.Strict    (MonadReader, MonadState, MonadWriter, RWS,
                                              ask, tell)
import           Data.Binary                 (Binary, decodeFile, encodeFile)
import qualified Data.ByteString             as BS
import qualified Data.Map                    as M
import           Data.Maybe                  (listToMaybe)
import           Data.Monoid                 ((<>))
import           Data.Time.Clock             (getCurrentTime)
import           Data.Typeable               (Typeable)
import           GHC.Generics                (Generic)

import           Communication               (PolyMessage (..), Sendable (..),
                                              SendableLike)
import           PaxosTypes                  (AcceptorState (..), LeaderState (..),
                                              ReplicaState (..), emptyAcceptorState,
                                              emptyLeaderState, emptyReplicaState)
import           Types                       (Host, Key, Port, Role, Value)

-- Readable

data ServerConfig = ServerConfig
    { serverPid      :: ProcessId
    , serverRoles    :: [Role]
    , serverJournal  :: FilePath
    , acceptorsN     :: Int
    , knownReplicas  :: [ProcessId]
    , knownAcceptors :: [ProcessId]
    , knownLeaders   :: [ProcessId]
    } deriving (Show)

-- Writable

data WriterPart = WriterPart
    { wMessages :: [PolyMessage]
    , wLogs     :: [String]
    }

instance Monoid WriterPart where
    mempty = WriterPart [] []
    mappend (WriterPart a b) (WriterPart d e) =
        WriterPart (a ++ d) (b ++ e)

writeMsg    :: (MonadWriter WriterPart m) => PolyMessage -> m ()
writeLog    :: (MonadWriter WriterPart m) => String -> m ()
writeMsg m = tell $ WriterPart [m] []
writeLog l = tell $ WriterPart [] [l]

-- Stateful

data ServerState = ServerState
    { _hashmap  :: M.Map Key Value
    , _replica  :: ReplicaState
    , _acceptor :: AcceptorState
    , _leader   :: LeaderState
    } deriving (Read,Generic,Typeable)
makeLenses ''ServerState

instance Binary ServerState

instance Show ServerState where
    show ServerState{..} =
        mconcat [ "ServerState {\n_hashmap = "
                , show _hashmap
                , ",\n_replica = "
                , show _replica
                , ",\n_acceptor = "
                , show _acceptor
                , ",\n_leader = "
                , show _leader
                , "}" ]

emptyServerState :: ServerState
emptyServerState =
    ServerState M.empty emptyReplicaState emptyAcceptorState emptyLeaderState

-- | Writes server state to the given path
dumpServerState :: (MonadIO m) => FilePath -> ServerState -> m ()
dumpServerState journalPath st = liftIO $ encodeFile journalPath st

-- | If not succeeds to read server state, creates an empty one, dumps
-- it and returns it
readServerState :: (MonadIO m) => FilePath -> m ServerState
readServerState journalPath = liftIO $ go `catch` fallback
  where
    go = do
        res <- decodeFile journalPath
        threadDelay 400000
        return res
    fallback (e :: SomeException) = do
        liftIO $ putStrLn $ "Creating emptyServerState, failed to read: " ++ show e
        dumpServerState journalPath emptyServerState
        return emptyServerState

-- RWS! :)

newtype ServerM a = ServerM
    { runServerM :: RWS ServerConfig WriterPart ServerState a
    } deriving (Functor,Applicative,Monad,MonadState ServerState,
                MonadWriter WriterPart,MonadReader ServerConfig)

writeMsg' :: (SendableLike a) => ProcessId -> a -> ServerM ()
writeMsg' p arg = writeMsg $ PolyMessage p $ Sendable arg
