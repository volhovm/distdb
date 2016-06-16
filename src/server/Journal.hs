-- | Module responsible for storing data in the text file on disk
module Journal
       ( getEntries
       , putEntry
       , deleteEntry
       ) where

import           Control.Monad.IO.Class (MonadIO (..), liftIO)
import           Data.List              (delete, nub)
import           System.IO              (Handle, IOMode (ReadWriteMode),
                                         hGetContents, withFile)

import           Types                  (Entry (..))

newtype Journal = Journal { journalPath :: FilePath }

formatEntry :: Entry -> String
formatEntry Entry{..} = eKey ++ " " ++ eValue

withJournalFile :: (MonadIO m) => Journal -> (Handle -> IO a) -> m a
withJournalFile Journal{..} = liftIO . withFile journalPath ReadWriteMode

getEntries :: (MonadIO m) => Journal -> m [Entry]
getEntries j = withJournalFile j $ \handle ->
    map toEntry . lines <$> hGetContents handle
  where
    toEntry x = let (k:v:_) = words x in Entry k v

overwriteJournal :: (MonadIO m) => Journal -> [Entry] -> m ()
overwriteJournal j entries =
    liftIO $ writeFile (journalPath j) $
        unlines $ map formatEntry $ nub entries

putEntry :: (MonadIO m) => Journal -> Entry -> m ()
putEntry j e = do
    deleteEntry j e
    liftIO $ appendFile (journalPath j) $ formatEntry e

-- | Deletes entry, returns [if the entry was there]
deleteEntry :: (MonadIO m) => Journal -> Entry -> m ()
deleteEntry j e = do
    entries <- getEntries j
    overwriteJournal j $ delete e $ nub entries
