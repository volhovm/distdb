{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (Process, ProcessId, getSelfPid, match,
                                                   receiveWait, register, send,
                                                   spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable)
import           Network.Socket                   (withSocketsDo)

import           Control.Lens                     (uses, (%=), (+=), (.=))
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.RWS.Strict         (ask, execRWS)
import           Data.Bifunctor                   (bimap)
import           Data.Binary                      (Binary (..))
import           Data.List                        (delete)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust)
import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import           System.Environment               (getArgs)


import           Communication                    (Message (..), PolyMessage (..),
                                                   Sendable (..), send')
import           ConfigFile                       (readConfig)
import           ServerTypes                      (ServerConfig (..), ServerM (..),
                                                   ServerState, WriterPart (..),
                                                   dumpServerState, hashmap, pongsNumber,
                                                   readServerState, serverPeers, writeLog,
                                                   writeMsg)
import           Types                            (Entry (..), EntryRequest (..),
                                                   EntryResponse (..), Host (getHost),
                                                   NetworkConfig (..), Pinging (..),
                                                   Port (getPort))


data Tick = Tick deriving (Show,Generic,Typeable)
instance Binary Tick

threadDelayMS s = threadDelay $ s * (1000 :: Int)
say' = liftIO . putStrLn

tickHandler :: Tick -> ServerM ()
tickHandler _ = do
    ServerConfig{..} <- ask
    let peersSize = length serverPeers
    pongsLess <- uses pongsNumber (< peersSize)
    when pongsLess $ writeLog "Not all pongs were here"
    pongsNumber .= 0
    forM_ serverPeers $ \p -> writeMsg $ PolyMessage p $ Sendable Ping
    return ()

pingingHandler :: Message Pinging -> ServerM ()
pingingHandler (Message from Ping) = do
    ServerConfig{..} <- ask
    void $ writeMsg $ PolyMessage from $ Sendable Pong
pingingHandler (Message _ Pong) =
    pongsNumber += 1

entryRequestHandler :: Message EntryRequest -> ServerM ()
entryRequestHandler (Message from (GetEntry k)) = do
    value <- uses hashmap (M.lookup k)
    let returnVal = maybe EntryNotFound (EntryFound . Entry k) value
    writeMsg $ PolyMessage from $ Sendable returnVal
    writeLog $ "Send " ++ show returnVal ++ " to " ++ show from
entryRequestHandler (Message from (SetEntry (Entry k v))) = do
    hashmap %= M.insert k v
    writeMsg $ PolyMessage from $ Sendable EntrySet
    writeLog $ "Send EntrySet to " ++ show from
entryRequestHandler (Message from (DeleteEntry k)) = do
    isMember <- uses hashmap (M.member k)
    hashmap %= M.delete k
    let returnVal = if isMember then EntryDeleted else EntryNotFound
    writeMsg $ PolyMessage from $ Sendable returnVal
    writeLog $ "Send " ++ show returnVal ++ " to " ++ show from

runServer :: ServerConfig -> ServerState -> Process ()
runServer config state = do
    -- TODO Don't forget to initialize leader!
    let run handler msg = return $ execRWS (runServerM $ handler msg) config state
    (state', writerPart) <-
        receiveWait [ match $ run tickHandler
                    , match $ run pingingHandler
                    , match $ run entryRequestHandler ]
    runWriterPart writerPart
    say' $ "Current state: " ++ show state'
    newPeers <- getNodes
    let updateConf ServerConfig{..} = ServerConfig { serverPeers = newPeers , ..}
        config' = updateConf config
    dumpServerState (serverJournal config') state'
    runServer config' state'
  where
    runWriterPart WriterPart{..} = do
        forM_ wMessages $ \PolyMessage{..} -> do
            say' "Sending a message to somebody"
            case msgBody' of (Sendable a) -> send' msgTo' a
        forM_ wLogs say'
        forM_ wIOActions liftIO

worker :: Int -> (Host, Port) -> NetworkConfig -> Process ()
worker index (host, port) NetworkConfig{..} = do
    say' "Waiting for nodes to appear"
    liftIO $ threadDelayMS 2000
    getSelfPid >>= register "distdbNode"
    myPid <- getSelfPid
    void $ spawnLocal $ forever $ do
        liftIO $ threadDelayMS 8000
        send myPid Tick
    nodes <- getNodes
    --serverState <- expect :: Process String
    let logFile = "distdb" ++ show index ++ ".log"
    serverState <- readServerState logFile
    runServer (ServerConfig myPid host port index logFile networkSize nodes)
              serverState

getNodes :: Process [ProcessId]
getNodes = do
    self <- getSelfPid
    delete self <$> P2P.getCapable "distdbNode"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    when (length args /= 1) $
        error "You should provide 1 argument with node id inside"
    let index = read $ head args
    putStrLn $ "Server called with index: " ++ show index
    conf@NetworkConfig{..} <- readConfig
    let nodeData = fromJust $ M.lookup index portMap
        !(getHost -> host, show . getPort -> port) = nodeData
        otherGuys = delete (host,port) $
                    map (bimap getHost (show . getPort)) $
                    M.elems portMap
        makeNode (h,p) = P2P.makeNodeId $ h ++ ":" ++ p
    putStrLn $ "Those: " ++ show (map makeNode otherGuys)
    P2P.bootstrap
        host
        port
        (map makeNode otherGuys)
        initRemoteTable
        (worker index nodeData conf)
