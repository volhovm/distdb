-- | Server command-line options

module ServerOptions
       ( ServerOptions (..)
       , getServerOptions
       ) where

import           Data.Monoid         ((<>))

import           Options.Applicative (Parser, auto, execParser, fullDesc, help,
                                      helper, info, long, option, progDesc,
                                      showDefault)

data ServerOptions = ServerOptions { nodeIndex :: Word }
                   deriving Show

serverOptionsParser :: Parser ServerOptions
serverOptionsParser =
    ServerOptions <$>
    option
        auto
        (long "index" <>
         help "Index of the server (corresponding to one in config file)" <>
         showDefault)

getServerOptions :: IO ServerOptions
getServerOptions =
    execParser $
    info
        (helper <*> serverOptionsParser)
        (fullDesc <> progDesc "DistDB user client")
