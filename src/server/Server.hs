{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (Process, ProcessId, expectTimeout,
                                                   getSelfPid, match, receiveWait,
                                                   register, send, spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable)
import           Network.Socket                   (withSocketsDo)

import           Control.Lens                     (use, uses, (%=), (+=), (.=))
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.RWS.Strict         (ask, execRWS)
import           Data.Bifunctor                   (bimap)
import           Data.Binary                      (Binary (..))
import           Data.List                        (delete, nub)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, isJust)
import           Data.Time.Clock                  (getCurrentTime)
import           Data.Tuple.Select                (sel3)
import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import           System.Environment               (getArgs)


import           Communication                    (Message (..), PolyMessage (..),
                                                   Sendable (..), SendableLike, send')
import           ConfigFile                       (readConfig)
import qualified PaxosLogic                       as L
import qualified PaxosTypes                       as L
import           ServerTypes                      (ServerConfig (..), ServerM (..),
                                                   ServerState (..), WriterPart (..),
                                                   dumpServerState, hashmap,
                                                   knownAcceptors, knownLeaders,
                                                   knownReplicas, readServerState,
                                                   replica, writeLog, writeMsg')
import           Types                            (Entry (..), EntryRequest (..),
                                                   EntryResponse (..), Host (getHost),
                                                   Key, NetworkConfig (..), Pinging (..),
                                                   Port (getPort), Role (..), Value)


data StateSharing = ShareStatePls | HereIsYourState L.ReplicaState (M.Map Key Value)
                    deriving (Show,Generic,Typeable)
instance Binary StateSharing
instance SendableLike StateSharing

data Tick = Tick deriving (Show,Generic,Typeable)
instance Binary Tick

threadDelayMS s = threadDelay $ s * (1000 :: Int)
say' = liftIO . putStrLn

getNodes :: Role -> Process [ProcessId]
getNodes = P2P.getCapable . roleToService

roleToService :: Role -> String
roleToService Leader = "distdbLeader"
roleToService Replica = "distdbReplica"
roleToService Acceptor = "distdbAcceptor"

stateSharingHandler :: Message StateSharing -> ServerM ()
stateSharingHandler (Message _ (HereIsYourState _ _)) = return () -- ignore
stateSharingHandler (Message from ShareStatePls) = do
    writeLog "State sharing requesting, performing"
    r <- use replica
    h <- use hashmap
    writeMsg' from $ HereIsYourState r h

tickHandler :: Tick -> ServerM ()
tickHandler = const $ return ()
--tickHandler _ = do
--    ServerConfig{..} <- ask
--    let peersSize = length serverPeers
--    forM_ serverPeers $ \p -> writeMsg $ PolyMessage p $ Sendable Ping
--    return ()

pingingHandler :: Message Pinging -> ServerM ()
pingingHandler (Message from Ping) = do
    writeLog $ "I was pinged by " ++ show from
    ServerConfig{..} <- ask
    void $ writeMsg' from Pong
pingingHandler (Message from Pong) =
    writeLog $ "I was ponged by " ++ show from

runWriterPart WriterPart{..} = do
    forM_ wLogs say'
    forM_ wMessages $ \PolyMessage{..} -> do
        say' "Sending a message to somebody"
        case msgBody' of (Sendable a) -> send' msgTo' a

updateConfig :: ServerConfig -> Process ServerConfig
updateConfig ServerConfig {..} = do
    a' <- unionL knownAcceptors <$> getNodes Acceptor
    r' <- unionL knownReplicas <$> getNodes Replica
    l' <- unionL knownLeaders <$> getNodes Leader
    say' "Updating config"
    return $ ServerConfig { knownReplicas = r'
                          , knownAcceptors = a'
                          , knownLeaders = l', ..}
  where
    unionL a b = nub $ a ++ b

runServer :: ServerConfig -> ServerState -> Process ()
runServer config state = do
    say' "Waiting for new messages (receive block)"
    (state',writerPart) <-
        receiveWait $
            concatMap handlersOf (serverRoles config) ++
            [
              match $ run tickHandler,
              match $ run pingingHandler]
    config' <- updateConfig config
    runWriterPart writerPart
    say' "\n"
    liftIO $ print =<< getCurrentTime
    --say' $ "Current state: " ++ show state'
    config'' <- updateConfig config'
    dumpServerState (serverJournal config'') state'
    runServer config'' state'
  where
    run handler msg =
        return $ execRWS (runServerM $ handler msg) config state
    handlersOf Leader =
        [match $ run L.leaderOnCommit,
         match $ run L.leaderOnNotification]
    handlersOf Acceptor = [match $ run L.acceptorOnPC]
    handlersOf Replica =
        [match $ run L.replicaOnRequest,
         match $ run L.replicaOnDecision,
         match $ run stateSharingHandler]

worker :: Int -> NetworkConfig -> Process ()
worker index NetworkConfig{..} = do
    say' "Waiting for nodes to appear"
    liftIO $ threadDelayMS 1000

    say' $ "Roles: " ++ show roles
    self <- getSelfPid

    config0 <- getInitConfig
    config <- updateConfig config0
    state0 <- retrieveState

    forM_ roles $ \r -> register (roleToService r) self

    void $ spawnLocal $ forever $ do
        liftIO $ threadDelayMS 30000
        send self Tick

    if Leader `elem` roles
    then do
        -- leader initialization
        let (state, w0) = execRWS (runServerM L.initLeader) config state0
        runWriterPart w0
        runServer config state
    else runServer config state0
  where
    roles = sel3 $ fromJust $ M.lookup index portMap
    logFile = "distdbNode" ++ show index ++ ".log"
    getInitConfig = do
        self <- getSelfPid
        return $ ServerConfig self roles logFile (M.size portMapAcceptors) [] [] []
    retrieveState :: Process ServerState
    retrieveState = do
      self <- getSelfPid
      otherReplicas <- getNodes Replica
      forM_ otherReplicas $ \repl -> send repl $ Message self ShareStatePls
      st0@ServerState{..} <- readServerState logFile
      mRes <- expectTimeout 2000000
      maybe (say' "Created state/reading from file" >> return st0)
            (\(Message from (HereIsYourState m h)) -> do
                say' $ "Successfully retrieved state from " ++ show from
                return ServerState { _hashmap = h, _replica = m, .. })
            mRes

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    when (length args /= 1) $
        error "You should provide 1 argument with node id inside"
    let index = read $ head args
    conf@NetworkConfig{..} <- readConfig
    putStrLn $ "Server called with index: " ++ show index
    let nodeData = fromJust $ M.lookup index portMap
        !(getHost -> host, show . getPort -> port, _) = nodeData
        otherGuys = delete (host,port) $
                    map (bimap getHost (show . getPort)) $
                    map (\(h,p,_) -> (h,p)) $
                    M.elems portMap
        makeNode (h,p) = P2P.makeNodeId $ h ++ ":" ++ p
    putStrLn $ "Those: " ++ show (map makeNode otherGuys)
    P2P.bootstrap
        host
        port
        (map makeNode otherGuys)
        initRemoteTable
        (worker index conf)
