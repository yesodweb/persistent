{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains utility function which works on the RDBMS backends of persistent. i.e. MySQL, Postgres and Sqlite.
--
-- * Reference Schema & Dataset
--
-- All the combinators present here will be explained based on this schema:
--
-- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- > User
-- >     name String
-- >     age Int
-- >     deriving Show
-- > |]
--
-- and this dataset. The examples below will refer to this as dataset-1.
--
-- #dataset#
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+
--
module Database.Persist.RDBMS.Utils
  ( dropTable
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader
import Database.Persist

-- | Deletes all rows in a database. Issues \"DELETE FROM table_name\" command to the database.
--
-- @
-- deleteUserRows :: MonadIO m => ReaderT SqlBackend m ()
-- deleteUserRows = dropTable (undefined :: User)
-- @
--
-- dropTable
--   :: (MonadIO m, PersistEntity record)
--   => record -> ReaderT SqlBackend m ()
dropTable
  :: (PersistEntityBackend record ~ BaseBackend backend
     ,PersistEntity record
     ,PersistQueryWrite backend
     ,MonadIO m)
  => record -> ReaderT backend m ()
dropTable entity = deleteWhere filts
  where
    filts = dummyFromFilts ent
    ent = Just entity

dummyFromFilts :: Maybe v -> [Filter v]
dummyFromFilts (Just _) = []
dummyFromFilts Nothing = []
