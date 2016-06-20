{-# LANGUAGE DeriveFunctor              #-}
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
       , writeMsg, writeLog, writeAction

       , ServerState (..)
       , pongsNumber, hashmap, replica
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
import qualified Data.Map                    as M
import           Data.Maybe                  (listToMaybe)
import           Data.Time.Clock             (getCurrentTime)

import           Communication               (PolyMessage (..), Sendable (..),
                                              SendableLike)
import           PaxosTypes                  (ReplicaState (..), emptyReplicaState)
import           Types                       (Host, Key, Port, Value)

-- Readable

data ServerConfig = ServerConfig
    { serverPid     :: ProcessId
    , serverHost    :: Host
    , serverPort    :: Port
    , serverId      :: Int
    , serverJournal :: FilePath
    , serverPeers   :: [ProcessId]
    } deriving (Show)

-- Writable

data WriterPart = WriterPart
    { wMessages  :: [PolyMessage]
    , wLogs      :: [String]
    , wIOActions :: [IO ()]
    }

instance Monoid WriterPart where
    mempty = WriterPart [] [] []
    mappend (WriterPart a b c) (WriterPart d e f) =
        WriterPart (a ++ d) (b ++ e) (c ++ f)

writeMsg    :: (MonadWriter WriterPart m) => PolyMessage -> m ()
writeLog    :: (MonadWriter WriterPart m) => String -> m ()
writeAction :: (MonadWriter WriterPart m) => IO () -> m ()
writeMsg m = tell $ WriterPart [m] [] []
writeLog l = tell $ WriterPart [] [l] []
writeAction a = tell $ WriterPart [] [] [a]

-- Stateful

data ServerState = ServerState
    { _pongsNumber :: Int
    , _hashmap     :: M.Map Key Value
    , _replica     :: ReplicaState
    } deriving (Show,Read)
makeLenses ''ServerState

emptyServerState :: ServerState
emptyServerState = ServerState 0 M.empty emptyReplicaState

-- | Writes server state to the given path
dumpServerState :: (MonadIO m) => FilePath -> ServerState -> m ()
dumpServerState journalPath st = liftIO $ do
    curDate <- getCurrentTime
    appendFile journalPath $ show curDate ++ "\n" ++ show st ++ "\n"

-- | If not succeeds to read server state, creates an empty one, dumps
-- it and returns it
readServerState :: (MonadIO m) => FilePath -> m ServerState
readServerState journalPath = liftIO $ go `catch` fallback
  where
    go = do
        maybeState <-
            fmap fst . listToMaybe . reads . last . words <$>
            readFile journalPath
        threadDelay 1000000
        maybe (error ":(") return maybeState
    fallback (_ :: SomeException) = do
        dumpServerState journalPath emptyServerState
        return emptyServerState

-- RWS! :)

newtype ServerM a = ServerM
    { runServerM :: RWS ServerConfig WriterPart ServerState a
    } deriving (Functor,Applicative,Monad,MonadState ServerState,
                MonadWriter WriterPart,MonadReader ServerConfig)

writeMsg' :: (SendableLike a) => ProcessId -> a -> ServerM ()
writeMsg' p arg = writeMsg $ PolyMessage p $ Sendable arg