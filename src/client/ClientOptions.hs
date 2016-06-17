-- | Client command line options parsing module

module ClientOptions
       ( ClientCommand (..)
       , ClientOptions (..)
       , getClientOptions
       ) where

import           Data.Monoid         ((<>))
import           Data.String         (fromString)
import           Options.Applicative (Parser, auto, command, execParser, fullDesc, help,
                                      helper, info, long, option, progDesc, short, str,
                                      strOption, subparser)

import           Types               (Entry (..), Host, Key, Port (..))

data ClientCommand
    = SetEntry Entry
    | GetEntry Key
    | DeleteEntry Key
    deriving (Show)

data ClientOptions = ClientOptions
    { clientCommand :: ClientCommand
    , clientHost    :: Host
    , clientPort    :: Port
    } deriving (Show)

userCommandParser :: Parser ClientCommand
userCommandParser =
    subparser
        (command "set" (info setOpts (progDesc "Set the <key, value>")) <>
         command "get" (info getOpts (progDesc "Get the <key> value")) <>
         command "delete" (info deleteOpts (progDesc "Delete the <key> value")))
  where
    keyDesc = "Key: ASCII string without spaces"
    valueDesc = "Value: ASCII string without spaces"
    setOpts =
        SetEntry <$>
        (Entry <$>
         option str (short 'k' <> long "key" <> help keyDesc) <*>
         option str (short 'v' <> long "value" <> help valueDesc))
    getOpts = GetEntry <$> option str (short 'k' <> long "key" <> help keyDesc)
    deleteOpts = DeleteEntry <$> option str (short 'k' <> long "key" <> help keyDesc)

userOptionsParser :: Parser ClientOptions
userOptionsParser =
    ClientOptions <$>
    userCommandParser <*>
    (fromString <$> strOption (short 'h' <> long "host" <> help "User host to bind on.")) <*>
    (Port <$> option auto (short 'p' <> long "port" <> help "User port to bind on."))

getClientOptions :: IO ClientOptions
getClientOptions =
    execParser $
    info
        (helper <*> userOptionsParser)
        (fullDesc <> progDesc "DistDB user client")
