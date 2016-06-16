-- | This module provides capabilities of reading global network
-- configuration file
module ConfigFile where

import           Control.Exception      (Exception)
import           Control.Monad          (when)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO (..), liftIO)
import           Data.Function          (on)
import           Data.List              (find, isPrefixOf, nubBy)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)

import           Types                  (Host (..), NetworkConfig (..),
                                         Port (..))

data ConfigException = ConfigException String
                       deriving Show

instance Exception ConfigException

-- | Reads config from the "./dkvs.properties" file
readConfig :: (MonadIO m, MonadThrow m) => m NetworkConfig
readConfig = do
    configLines <-
        liftIO $ map (span (/= '=')) . lines <$> readFile "./dkvs.properties"
    let timeoutValue = read . snd . fromJust $ find ((== "timeout") . fst) configLines
        nodeConfigLines = filter (("node." `isPrefixOf`) . fst) configLines
        readNodeLine (title,option) =
            let nodeN = read $ drop 5 title
                (hostS,portS) = span (/= ':') option
                in (nodeN, (Host hostS, Port $ read portS))
        nodesConfig :: [(Int, (Host,Port))]
        nodesConfig = nubBy ((==) `on` fst) $ map readNodeLine nodeConfigLines
        maxNumber = maximum $ map fst nodesConfig
    when (any (< 1) $ map fst nodesConfig) $
        throwM $ ConfigException
        "There are nodes with negative index in config"
    when (length nodesConfig /= maxNumber) $
        throwM $ ConfigException
        "Maximum node number doesn't equal length of nodes descriptions in config"
    return
        NetworkConfig
        { networkSize = maxNumber
        , portMap = M.fromList nodesConfig
        , timeout = timeoutValue
        }
