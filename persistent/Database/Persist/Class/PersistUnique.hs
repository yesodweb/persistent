{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Database.Persist.Class.PersistUnique
    ( PersistUnique (..)
    , getByValue
    , insertBy
    , replaceUnique
    , checkUnique
    , onlyUnique
    ) where

import Database.Persist.Types
import qualified Prelude
import Prelude hiding ((++))

import Control.Exception (throwIO)
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))

import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.IO.Class (MonadIO)

import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity
import Data.Monoid (mappend)
import Data.Text (unpack, Text)

-- | Queries against 'Unique' keys (other than the id 'Key').
--
-- Please read the general Persistent documentation to learn how to create
-- 'Unique' keys.
--
-- Using this with an Entity without a Unique key leads to undefined behavior.
-- A few of these functions require a *single* 'Unique', so using an Entity with multiple 'Unique's is also undefined. In these cases persistent's goal is to throw an exception as soon as possible, but persistent is still transitioning to that.
--
-- SQL backends automatically create uniqueness constraints, but for MongoDB you must manually place a unique index on a field to have a uniqueness constraint.
--
-- Some functions in this module (insertUnique, insertBy, and replaceUnique) first query the unique indexes to check for conflicts.
-- You could instead optimistically attempt to perform the operation (e.g. replace instead of replaceUnique). However,
--
--  * there is some fragility to trying to catch the correct exception and determing the column of failure.
--
--  * an exception will automatically abort the current SQL transaction
class PersistStore backend => PersistUnique backend where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Unique val -> ReaderT backend m (Maybe (Entity val))

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: (MonadIO m, PersistEntityBackend val ~ backend, PersistEntity val) => Unique val -> ReaderT backend m ()

    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique :: (MonadIO m, PersistEntityBackend val ~ backend, PersistEntity val) => val -> ReaderT backend m (Maybe (Key val))
    insertUnique datum = do
        conflict <- checkUnique datum
        case conflict of
          Nothing -> Just `liftM` insert datum
          Just _ -> return Nothing

    -- | update based on a uniquness constraint or insert
    --
    -- insert the new record if it does not exist
    -- update the existing record that matches the uniqueness contraint
    --
    -- Throws an exception if there is more than 1 uniqueness contraint
    upsert :: (MonadIO m, PersistEntityBackend val ~ backend, PersistEntity val)
           => val          -- ^ new record to insert
           -> [Update val] -- ^ updates to perform if the record already exists.
                           -- leaving this empty is the equivalent of performing a 'repsert' on a unique key.
           -> ReaderT backend m (Entity val) -- ^ the record in the database after the operation
    upsert record updates = do
        uniqueKey <- onlyUnique record 
        mExists <- getBy uniqueKey
        k <- case mExists of
            Just (Entity k _) -> do
              when (null updates) (replace k record)
              return k
            Nothing           -> insert record
        Entity k `liftM` updateGet k updates


-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
insertBy :: (MonadIO m, PersistEntity val, PersistUnique backend, PersistEntityBackend val ~ backend)
         => val -> ReaderT backend m (Either (Entity val) (Key val))
insertBy val = do
    res <- getByValue val
    case res of
      Nothing -> Right `liftM` insert val
      Just z -> return $ Left z

-- | Return the single unique key for a record
onlyUnique :: (MonadIO m, PersistEntity val, PersistUnique backend, PersistEntityBackend val ~ backend)
           => val -> ReaderT backend m (Unique val)
onlyUnique record = case onlyUniqueEither record of
    Right u -> return u
    Left us -> requireUniques record us >>= liftIO . throwIO . OnlyUniqueException . show . length

onlyUniqueEither :: (PersistEntity val) => val -> Either [Unique val] (Unique val)
onlyUniqueEither record = case persistUniqueKeys record of
    (u:[]) -> Right u
    us     -> Left us

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' record. Returns a record matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue :: (MonadIO m, PersistEntity record, PersistUnique backend, PersistEntityBackend record ~ backend)
           => record -> ReaderT backend m (Maybe (Entity record))
getByValue record = checkUniques =<< requireUniques record (persistUniqueKeys record)
  where
    checkUniques [] = return Nothing
    checkUniques (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> checkUniques xs
            Just z -> return $ Just z

requireUniques :: (MonadIO m, PersistEntity record) => record -> [Unique record] -> m [Unique record]
requireUniques record [] = liftIO $ throwIO $ userError errorMsg
  where
    errorMsg = "getByValue: " `mappend` unpack (recordName record) `mappend` " does not have any Unique"
requireUniques _ xs = return xs

-- TODO: expose this to users
recordName :: (PersistEntity record) => record -> Text
recordName = unHaskellName . entityHaskell . entityDef . Just

-- | Attempt to replace the record of the given key with the given new record.
-- First query the unique fields to make sure the replacement maintains uniqueness constraints.
-- Return 'Nothing' if the replacement was made.
-- If uniqueness is violated, return a 'Just' with the 'Unique' violation
--
-- Since 1.2.2.0
replaceUnique :: (MonadIO m, Eq record, Eq (Unique record), PersistEntityBackend record ~ backend, PersistEntity record, PersistUnique backend)
              => Key record -> record -> ReaderT backend m (Maybe (Unique record))
replaceUnique key datumNew = getJust key >>= replaceOriginal
  where
    uniqueKeysNew = persistUniqueKeys datumNew
    replaceOriginal original = do
        conflict <- checkUniqueKeys changedKeys
        case conflict of
          Nothing -> replace key datumNew >> return Nothing
          (Just conflictingKey) -> return $ Just conflictingKey
      where
        changedKeys = uniqueKeysNew \\ uniqueKeysOriginal
        uniqueKeysOriginal = persistUniqueKeys original

-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'Nothing' if the entity would be unique, and could thus safely be inserted.
-- on a conflict returns the conflicting key
checkUnique :: (MonadIO m, PersistEntityBackend record ~ backend, PersistEntity record, PersistUnique backend)
            => record -> ReaderT backend m (Maybe (Unique record))
checkUnique = checkUniqueKeys . persistUniqueKeys

checkUniqueKeys :: (MonadIO m, PersistEntity record, PersistUnique backend, PersistEntityBackend record ~ backend)
                => [Unique record] -> ReaderT backend m (Maybe (Unique record))
checkUniqueKeys [] = return Nothing
checkUniqueKeys (x:xs) = do
    y <- getBy x
    case y of
        Nothing -> checkUniqueKeys xs
        Just _ -> return (Just x)
