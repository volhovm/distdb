-- | Module responsible for storing data in the text file on disk
module Journal
       ( getEntries
       , putEntry
       , getByKey
       , deleteByKey
       ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..), liftIO)
import           Data.Function          (on)
import           Data.List              (find, nubBy)
import           Data.Maybe             (isJust)
import           System.IO              (Handle, IOMode (ReadWriteMode),
                                         hGetContents, withFile)

import           Types                  (Entry (..))

newtype Journal = Journal { journalPath :: FilePath }

withJournalFile :: (MonadIO m) => Journal -> (Handle -> IO a) -> m a
withJournalFile Journal{..} = liftIO . withFile journalPath ReadWriteMode

getEntries :: (MonadIO m) => Journal -> m [Entry]
getEntries j = withJournalFile j $ \handle ->
    map read . lines <$> hGetContents handle

overwriteJournal :: (MonadIO m) => Journal -> [Entry] -> m ()
overwriteJournal j entries =
    liftIO $ writeFile (journalPath j) $
        unlines $ map show $ nubBy ((==) `on` eKey) entries

getByKey :: (MonadIO m) => Journal -> String -> m (Maybe Entry)
getByKey j k = find (\x -> eKey x == k) <$> getEntries j

putEntry :: (MonadIO m) => Journal -> Entry -> m ()
putEntry j e = do
    void $ deleteByKey j $ eKey e
    liftIO $ appendFile (journalPath j) $ show e

-- | Deletes entry, returns [if the entry was there]
deleteByKey :: (MonadIO m) => Journal -> String -> m Bool
deleteByKey j k = do
    entries <- getEntries j
    let isThere = isJust $ find ((== k) . eKey) entries
    overwriteJournal j $ filter ((== k) . eKey) entries
    return isThere
