{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (Process, ProcessId, expect, getSelfPid,
                                                   match, processNodeId, receiveWait,
                                                   register, say, send, spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable, newLocalNode,
                                                   runProcess)
import           Network.Socket                   (withSocketsDo)
import           Network.Transport.TCP            (createTransport, defaultTCPParameters)


import           Control.Exception                (SomeException (..), bracket, catch)
import           Control.Lens                     (makeLenses, use, uses, view, (&), (+=),
                                                   (.=), (.~))
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.Catch              (throwM)
import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.RWS.Strict         (MonadReader, MonadState, MonadWriter,
                                                   RWS (..), ask, execRWS, tell)
import           Data.Bifunctor                   (bimap)
import           Data.Binary                      (Binary (..))
import           Data.List                        (delete, (\\))
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, isJust, isNothing,
                                                   listToMaybe)
import           Data.Time.Clock                  (getCurrentTime)
import           Data.Typeable                    (Typeable)
import           Debug.Trace
import           GHC.Generics                     (Generic)
import           System.Environment               (getArgs)


import           Communication                    (Message (..), PolyMessage (..),
                                                   Sendable (..), send')
import           ConfigFile                       (readConfig)
import qualified ServerOptions                    as O
import           Types                            (EntryRequest (..), EntryResponse (..),
                                                   Host (getHost), Key,
                                                   NetworkConfig (..), Pinging (..),
                                                   Port (getPort), Value)


data ServerConfig = ServerConfig
    { serverPid     :: ProcessId
    , serverHost    :: Host
    , serverPort    :: Port
    , serverId      :: Int
    , serverJournal :: FilePath
    , serverPeers   :: [ProcessId]
    } deriving (Show)

data ServerState = ServerState
    { _isLeader    :: Bool
    , _pongsNumber :: Int
    , _hashmap     :: M.Map Key Value
    } deriving (Show, Read)
makeLenses ''ServerState

emptyServerState :: ServerState
emptyServerState = ServerState False 0 M.empty

-- | Writes server state to the given path
dumpServerState :: (MonadIO m) => FilePath -> ServerState -> m ()
dumpServerState journalPath st = liftIO $ do
    curDate <- getCurrentTime
    appendFile journalPath $ (show curDate) ++ "\n" ++ show st ++ "\n"

-- | If not succeeds to read server state, creates an empty one, dumps
-- it and returns it
readServerState :: (MonadIO m) => FilePath -> m ServerState
readServerState journalPath =
    liftIO $ go `catch` fallback
  where
    go = do
        maybeState <- fmap fst . listToMaybe . reads . last . words <$>
                      readFile journalPath
        threadDelayMS 1000
        maybe (error ":(") return maybeState
    fallback (e :: SomeException) = do
            dumpServerState journalPath emptyServerState
            return emptyServerState

data WriterPart = WriterPart
    { wMessages  :: [PolyMessage]
    , wLogs      :: [String]
    , wIOActions :: [IO ()]
    }

instance Monoid WriterPart where
    mempty = WriterPart [] [] []
    mappend (WriterPart a b c) (WriterPart d e f) =
        WriterPart (a ++ d) (b ++ e) (c ++ f)

writeMsg m = tell $ WriterPart [m] [] []
writeLog l = tell $ WriterPart [] [l] []
writeAction a = tell $ WriterPart [] [] [a]

newtype ServerM a = ServerM
    { runServerM :: RWS ServerConfig WriterPart ServerState a
    } deriving (Functor,Applicative,Monad,MonadState ServerState,
                MonadWriter WriterPart,MonadReader ServerConfig)

data Tick = Tick deriving (Show,Generic,Typeable)
instance Binary Tick

threadDelayMS s = threadDelay $ s * (1000 :: Int)
say' = liftIO . putStrLn

tickHandler :: Tick -> ServerM ()
tickHandler _ = do
    ServerConfig{..} <- ask
    let peersSize = length serverPeers
    pongsLess <- uses pongsNumber $ (< peersSize)
    when (pongsLess) $ writeLog "Not all pongs were here"
    pongsNumber .= 0
    forM_ serverPeers $ \p -> writeMsg $ PolyMessage p $ Sendable Ping
    return ()

pingingHandler :: Message Pinging -> ServerM ()
pingingHandler (Message from Ping) = do
    ServerConfig{..} <- ask
    void $ writeMsg $ PolyMessage from $ Sendable Pong
pingingHandler (Message from Pong) = do
    pongsNumber += 1

entryRequestHandler :: Message EntryRequest -> ServerM ()
entryRequestHandler (Message from _) = do
    writeMsg $ PolyMessage from $ Sendable EntryNotFound
    writeLog $ "Send EntryNotFound to " ++ show from

runServer :: ServerConfig -> ServerState -> Process ()
runServer config state = do
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
worker index (host, port) conf@NetworkConfig{..} = do
    say' "Waiting for nodes to appear"
    liftIO $ threadDelayMS 2000
    getSelfPid >>= register "distdbNode"
    myPid <- getSelfPid
    void $ spawnLocal $ forever $ do
        liftIO $ threadDelayMS 8000
        send myPid Tick
    nodes <- getNodes
    let logFile = "distdb" ++ show index ++ ".log"
    serverState <- readServerState logFile
    runServer (ServerConfig myPid host port index logFile nodes)
              serverState
--    pids <- getOthers
--    forever $ do
--        when (port == "3551") $ do
--            say' "Expecting))"
--            hello <- expect :: Process String
--            say' $ hello
--        node9001 <- head <$> P2P.getCapable "3551"
--        send node9001 $ "Lol!!! Hello from " ++ port
--        liftIO $ threadDelayMS $ if port == "3551" then 500 else 3000
--  where

getNodes :: Process [ProcessId]
getNodes = do
    self <- getSelfPid
    delete self <$> P2P.getCapable "distdbNode"

main :: IO ()
main = do
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
        makeNode (h,p) = P2P.makeNodeId $ (h ++ ":" ++ p)
    putStrLn $ "Those: " ++ (show $ map makeNode otherGuys)
    P2P.bootstrap
        host
        port
        (map makeNode otherGuys)
        initRemoteTable
        (worker index nodeData conf)

{-
    let allPorts = ["9001", "9002", "9003"]
    [port] <- getArgs
    P2P.bootstrap "localhost" port
        (map (P2P.makeNodeId . ("localhost:" ++)) $ delete port allPorts) initRemoteTable $ do
-}
