module Main where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (Process, ProcessId, expect, getSelfPid,
                                                   match, processNodeId, receiveWait,
                                                   register, say, send, spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable, newLocalNode,
                                                   runProcess)
import           Control.Monad                    (forM_, forever, void, when)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Bifunctor                   (bimap)
import qualified Data.Map                         as M

import qualified ClientOptions                    as O
import           Communication                    (Message (..), PolyMessage (..),
                                                   Sendable (..), send')
import           ConfigFile                       (readConfig)
import           Types                            (EntryRequest (..), Host (getHost),
                                                   NetworkConfig (..), Pinging (..),
                                                   Port (getPort))

threadDelayMS s = threadDelay $ s * (1000 :: Int)

worker :: (Host, Port) -> NetworkConfig -> Process ()
worker (host, port) conf@NetworkConfig{..} =
    forever $ do
        say $ ("kek" :: String)
        liftIO $ threadDelayMS 3000

main :: IO ()
main = do
    o@O.ClientOptions{..} <- O.getClientOptions
    putStrLn $ "Called with options: " ++ show o
    conf@NetworkConfig{..} <- readConfig
    let makeNode (h,p) = P2P.makeNodeId $ (h ++ ":" ++ p)
        nodes = map (makeNode . bimap getHost (show . getPort)) $
                    M.elems portMap
    putStrLn $ "Connecting to: " ++ show nodes
    P2P.bootstrap
        (getHost clientHost)
        (show $ getPort clientPort)
        nodes
        initRemoteTable
        (worker (clientHost, clientPort) conf)
