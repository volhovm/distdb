{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
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


import           Control.Exception                (bracket, catch)
import           Control.Lens                     (makeLenses, use, uses, view, (&), (+=),
                                                   (.=), (.~))
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.RWS.Strict         (MonadReader, MonadState, MonadWriter,
                                                   RWS (..), ask, execRWS, tell)
import           Data.Bifunctor                   (bimap)
import           Data.Binary                      (Binary (..))
import           Data.List                        (delete, (\\))
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust)
import           Data.Typeable                    (Typeable)
import           Debug.Trace
import           GHC.Generics                     (Generic)
import           System.Environment               (getArgs)

import           ConfigFile                       (readConfig)
import qualified ServerOptions                    as O
import           Types                            (EntryRequest (..), Host (getHost),
                                                   NetworkConfig (..), Pinging (..),
                                                   Port (getPort))

-- Explicitely typed message
data Message a = Message
    { msgFrom :: ProcessId
    , msgBody :: a
    } deriving (Show,Generic,Typeable)

instance (Binary a) => Binary (Message a)

class (Binary a, Show a, Typeable a) => SendableLike a where
    send' :: ProcessId -> a -> Process ()
    send' pid x = send pid $ Message pid x

instance SendableLike Pinging
instance SendableLike EntryRequest

data Sendable = forall a . SendableLike a => Sendable { unSendable :: a }

instance Show Sendable where
    show (Sendable a) = "Sendable wrapper of: { " ++ show a ++ " }"

data ServerConfig = ServerConfig
    { serverPid  :: ProcessId
    , serverHost :: Host
    , serverPort :: Port
    , serverId   :: Int
    } deriving (Show)

data ServerState = ServerState
    { _isLeader    :: Bool
    , _pongsNumber :: Int
    , _peers       :: [ProcessId]
    } deriving (Show)
makeLenses ''ServerState

-- Polymorphic message
data PolyMessage = PolyMessage
    { msgTo'   :: ProcessId
    , msgBody' :: Sendable
    } deriving (Show)

newtype ServerM a = ServerM
    { runServerM :: RWS ServerConfig [PolyMessage] ServerState a
    } deriving (Functor,Applicative,Monad,MonadState ServerState,
                MonadWriter [PolyMessage],MonadReader ServerConfig)

data Tick = Tick deriving (Show,Generic,Typeable)
instance Binary Tick

threadDelayMS s = threadDelay $ s * (1000 :: Int)
say' = liftIO . putStrLn
--
--getOthers :: Process [ProcessId]
--getOthers = do
--    pid <- processNodeId <$> getSelfPid
--    peers <- P2P.getPeers
--    return $ delete pid peers

pingingHandler :: Message Pinging -> ServerM ()
pingingHandler (Message from Ping) = do
    ServerConfig{..} <- ask
    void $ tell [PolyMessage from $ Sendable Pong]
pingingHandler (Message from Pong) = do
    pongsNumber += 1

tickHandler :: Tick -> ServerM ()
tickHandler _ = do
    curPeers <- use peers
    peersSize <- uses peers length
    pongsLess <- uses pongsNumber $ (< peersSize)
    when (pongsLess) $ traceM "Not all pongs were here))"
    pongsNumber .= 0
    void $ tell $ map (\p -> PolyMessage p $ Sendable Ping) curPeers

runServer :: ServerConfig -> ServerState -> Process ()
runServer config state = do
    let run handler msg = return $ execRWS (runServerM $ handler msg) config state
    (state' :: ServerState, sendMessages :: [PolyMessage]) <-
        receiveWait [ match $ run tickHandler
                    , match $ run pingingHandler ]
    say' $ "Current state: " ++ show state'
    forM_ sendMessages $ \PolyMessage{..} -> case msgBody' of
                             (Sendable a) -> send' msgTo' a
    newPeers <- getNodes
    runServer config $ state' & peers .~ newPeers

worker :: Int -> (Host, Port) -> NetworkConfig -> Process ()
worker index (host, port) conf@NetworkConfig{..} = do
    liftIO $ threadDelayMS 2000
    getSelfPid >>= register "distdbNode"
    myPid <- getSelfPid
    void $ spawnLocal $ forever $ do
        liftIO $ threadDelayMS 8000
        send myPid Tick
    nodes <- getNodes
    runServer (ServerConfig myPid host port index) (ServerState (index == 1) 0 nodes)
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
