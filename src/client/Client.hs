module Main where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (NodeId, Process, ProcessId, expect,
                                                   expectTimeout, getSelfPid, match,
                                                   processNodeId, receiveWait, register,
                                                   say, send, spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable, newLocalNode,
                                                   runProcess)
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Bifunctor                   (bimap)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, isNothing)

import qualified ClientOptions                    as O
import           Communication                    (Message (..), PolyMessage (..),
                                                   Sendable (..), send')
import           ConfigFile                       (readConfig)
import           Types                            (EntryRequest (..), EntryResponse (..),
                                                   Host (getHost), NetworkConfig (..),
                                                   Pinging (..), Port (getPort),
                                                   responseMatches)

say' = liftIO . putStrLn

data ClientConfig = ClientConfig
    { swarm    :: [NodeId]
    , cHost    :: Host
    , cPort    :: Port
    , cTimeout :: Int
    , cRequest :: EntryRequest
    } deriving (Show)

threadDelayMS s = threadDelay $ s * (1000 :: Int)

worker :: ClientConfig -> Process ()
worker ClientConfig{..} = do
    liftIO $ threadDelayMS 3000
    self <- getSelfPid
    capable <- P2P.getCapable "distdbNode"
    if null capable
    then error "Couldn't send a request -- no nodes are available"
    else forM_ capable $ \node -> do
      send node $ Message self cRequest
      say' $ "Send message to " ++ show node
      (value :: Maybe (Message EntryResponse)) <- expectTimeout $ cTimeout * 1000
      let response = fromJust value
      when (isNothing value) $ error "Reached timeout limit, no answer from server"
      when (not $ responseMatches cRequest $ msgBody response) $
          error $ concat [ "Response doesn't match request: sent "
                         , show cRequest
                         , ", but received "
                         , show response ]
      liftIO $ putStrLn $ "Success, your result: " ++ show response

main :: IO ()
main = do
    o@O.ClientOptions{..} <- O.getClientOptions
    putStrLn $ "Called with options: " ++ show o
    NetworkConfig{..} <- readConfig
    let makeNode (h,p) = P2P.makeNodeId $ (h ++ ":" ++ p)
        nodes = map (makeNode . bimap getHost (show . getPort)) $
                    M.elems portMap
    putStrLn $ "Connecting to: " ++ show nodes
    P2P.bootstrap
        (getHost clientHost)
        (show $ getPort clientPort)
        nodes
        initRemoteTable
        (worker $ ClientConfig
                      nodes
                      clientHost
                      clientPort
                      timeout
                      clientCommand)
    putStrLn "Exiting"
