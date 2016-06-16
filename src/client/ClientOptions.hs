-- | Client command line options parsing module

module ClientOptions
       ( ClientCommand (..)
       , ClientOptions (..)
       , getClientOptions
       ) where

import           Data.Monoid         ((<>))
import           Options.Applicative (Parser, auto, command, execParser,
                                      fullDesc, help, helper, info, long,
                                      option, progDesc, subparser)

import           Types               (Entry (..), Key)


data ClientCommand
    = SetEntry Entry
    | GetEntry Key
    | DeleteEntry Key

data ClientOptions = ClientOptions { clientCommand :: ClientCommand }

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
         option auto (long "key" <> help keyDesc) <*>
         option auto (long "value" <> help valueDesc))
    getOpts = GetEntry <$> option auto (long "key" <> help keyDesc)
    deleteOpts = DeleteEntry <$> option auto (long "key" <> help keyDesc)

userOptionsParser :: Parser ClientOptions
userOptionsParser = ClientOptions <$> userCommandParser

getClientOptions :: IO ClientOptions
getClientOptions =
    execParser $
    info
        (helper <*> userOptionsParser)
        (fullDesc <> progDesc "DistDB user client")
