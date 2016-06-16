module Main where

import           Control.Exception          (bracket, catch)
import           Control.Monad              (forever)

import           System.Socket              (ReuseAddress (..), Socket,
                                             SocketException, SocketOption (..),
                                             accept, bind, close, listen,
                                             msgNoSignal, setSocketOption,
                                             socket)
import           System.Socket.Family.Inet6 (Inet6,
                                             SocketAddress (SocketAddressInet6),
                                             V6Only (..), inet6Any)
import           System.Socket.Protocol.TCP (TCP)
import           System.Socket.Type.Stream  (Stream, sendAll)

import qualified ServerOptions              as O

main :: IO ()
main = do
    o@O.ServerOptions{..} <- O.getServerOptions
    putStrLn $ "Server called with options: " ++ show o

pretendServer :: IO ()
pretendServer = bracket
    (socket :: IO (Socket Inet6 Stream TCP))
    (\s -> do close s
              putStrLn "Listening socket closed.")
    (\s -> do setSocketOption s (ReuseAddress True)
              setSocketOption s (V6Only False)
              bind s (SocketAddressInet6 inet6Any 8080 0 0)
              listen s 5
              putStrLn "Listening socket ready..."
              forever $
                  acceptAndHandle s `catch`
                  \e -> print (e :: SocketException))

acceptAndHandle :: Socket Inet6 Stream TCP -> IO ()
acceptAndHandle s =
    bracket
        (accept s)
        (\(p,addr) ->
              do close p
                 putStrLn $ "Closed connection to " ++ show addr)
        (\(p,addr) ->
              do putStrLn $ "Accepted connection from " ++ show addr
                 sendAll p "Hello world!" msgNoSignal)
