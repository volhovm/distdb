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

import           Types                  (Entry (..), Key)

newtype Journal = Journal { journalPath :: FilePath }

withJournalFile :: (MonadIO m) => Journal -> (Handle -> IO a) -> m a
withJournalFile Journal{..} = liftIO . withFile journalPath ReadWriteMode

-- | Returns the list of entries in the storage
getEntries :: (MonadIO m) => Journal -> m [Entry]
getEntries j = withJournalFile j $ \handle ->
    map read . lines <$> hGetContents handle

-- | Writes the entries to the storage, overwriting everything else
overwriteJournal :: (MonadIO m) => Journal -> [Entry] -> m ()
overwriteJournal j entries =
    liftIO $ writeFile (journalPath j) $
        unlines $ map show $ nubBy ((==) `on` eKey) entries

-- | Searches the entry with the given key
getByKey :: (MonadIO m) => Journal -> Key -> m (Maybe Entry)
getByKey j k = find (\x -> eKey x == k) <$> getEntries j

-- | Puts an entry in the set, removes everything with that key
putEntry :: (MonadIO m) => Journal -> Entry -> m ()
putEntry j e = do
    void $ deleteByKey j $ eKey e
    liftIO $ appendFile (journalPath j) $ show e

-- | Deletes entry, returns [if the entry was there]
deleteByKey :: (MonadIO m) => Journal -> Key -> m Bool
deleteByKey j k = do
    entries <- getEntries j
    let isThere = isJust $ find ((== k) . eKey) entries
    overwriteJournal j $ filter ((== k) . eKey) entries
    return isThere
