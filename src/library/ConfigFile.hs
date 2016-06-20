-- | This module provides capabilities of reading global network
-- configuration file
module ConfigFile (readConfig) where

import           Control.Exception      (Exception)
import           Control.Monad          (when)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO (..), liftIO)
import           Data.Function          (on)
import           Data.List              (find, isPrefixOf, nubBy)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Tuple.Select      (sel3)

import           Types                  (Host (..), NetworkConfig (..), Port (..),
                                         Role (..))

data ConfigException = ConfigException String
                       deriving Show

instance Exception ConfigException

-- | Reads config from the "./dkvs.properties" file
readConfig :: (MonadIO m, MonadThrow m) => m NetworkConfig
readConfig = do
    configLines <- liftIO $
        map (span' '=') . lines <$> readFile "./dkvs.properties"
    let timeout = read . snd . fromJust $ find ((== "timeout") . fst) configLines
        nodeConfigLines = filter (("node." `isPrefixOf`) . fst) configLines
        readNodeLine (title,option) =
            let (nodeN, role) = span' '.' $ drop 5 title
                (hostS,portS) = span' ':' option
                in (read nodeN, (Host hostS, Port $ read portS, map toRole role))
        portMap :: M.Map Int (Host,Port,[Role])
        portMap = M.fromList $ map readNodeLine nodeConfigLines
        maxNumber = maximum $ M.keys portMap
    when (any (< 1) $ M.keys portMap) $
        throwM $ ConfigException
        "There are nodes with negative index in config"
    when (M.size portMap /= maxNumber) $
        throwM $ ConfigException
        "Maximum node number doesn't equal length of nodes descriptions in config"
    let filterParty p =
            M.map (\(a,b,_) -> (a,b)) $
            M.filter (\(_,_,p') -> p `elem` p') portMap
    return
        NetworkConfig
        { portMapReplicas = filterParty Replica
        , portMapAcceptors = filterParty Acceptor
        , portMapLeaders = filterParty Leader
        , .. }
  where
    span' c = fmap (drop 1) . span (/= c)
    toRole 'r' = Replica
    toRole 'a' = Acceptor
    toRole 'l' = Leader
