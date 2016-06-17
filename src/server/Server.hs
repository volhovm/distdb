{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (Process, ProcessId, expect,
                                                   getSelfPid, processNodeId,
                                                   receiveWait, register, say,
                                                   send, spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable,
                                                   newLocalNode, runProcess)
import           Network.Socket                   (withSocketsDo)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)

import           Control.Exception                (bracket, catch)
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.RWS                (RWS (..), execRWS)
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
import           Types                            (EntryRequest (..),
                                                   Host (getHost),
                                                   NetworkConfig (..),
                                                   Pinging (..), Port (getPort))

class (Binary a, Typeable a) => SendableLike a where
    send' :: ProcessId -> a -> Process ()
    send' pid x = send pid x

instance SendableLike Pinging
instance SendableLike EntryRequest

data Sendable = forall a . SendableLike a => Sendable { unSendable :: a }

data ServerConfig = ServerConfig
    { serverID   :: ProcessId
    , serverHost :: Host
    , serverPort :: Port
    , serverId   :: Int
    }

data ServerState = ServerState
    { isLeader :: Bool
    } deriving (Show)

data Message = Message
   { messageFrom :: ProcessId
   , messageTo   :: ProcessId
   , msg         :: Sendable
   }

newtype ServerM a = ServerM { runServerM :: RWS ServerConfig [Message] ServerState a }

data Tick = Tick deriving (Show,Generic,Typeable)
instance Binary Tick

threadDelayMS s = threadDelay $ s * (1000 :: Int)
say' = liftIO . putStrLn

runServer :: ServerConfig -> ServerState -> Process ()
runServer config state = do
    let run handler msg = return $ execRWS (runServerM $ handler msg) config state
    (state' :: ServerState, sendMessages :: [Message]) <- receiveWait []
    say' $ "Current state: " ++ show state'
    forM_ sendMessages $ \Message{..} -> case msg of
        (Sendable a) -> send' messageTo a
    runServer config state'

worker :: Int -> (Host, Port) -> NetworkConfig -> Process ()
worker index (host, port) conf@NetworkConfig{..} = do
    liftIO $ threadDelayMS 2000
    getSelfPid >>= register (show $ getPort port)
    myPid <- getSelfPid
    void $ spawnLocal $ forever $ do
        liftIO $ threadDelayMS 5
        send myPid Tick
    runServer (ServerConfig myPid host port index) (ServerState $ index == 1)
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

getOthers = do
    pid <- processNodeId <$> getSelfPid
    peers <- P2P.getPeers
    return $ delete pid peers

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
