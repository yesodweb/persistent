{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstraintKinds #-}

module Database.Persist.Class.PersistUnique
  (PersistUniqueRead(..)
  ,PersistUniqueWrite(..)
  ,getByValue
  ,insertBy
  ,repsertBy
  ,insertUniqueEntity
  ,replaceUnique
  ,checkUnique
  ,onlyUnique
  ,defaultPutMany
  ,persistUniqueKeyValues
  )
  where

import Database.Persist.Types
import Control.Exception (throwIO)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.List ((\\), deleteFirstsBy, nubBy)
import Data.Function (on)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity
import Data.Monoid (mappend)
import Data.Text (unpack, Text)
import Data.Maybe (catMaybes)

-- | Queries against 'Unique' keys (other than the id 'Key').
--
-- Please read the general Persistent documentation to learn how to create
-- 'Unique' keys.
--
-- Using this with an Entity without a Unique key leads to undefined
-- behavior.  A few of these functions require a /single/ 'Unique', so using
-- an Entity with multiple 'Unique's is also undefined. In these cases
-- persistent's goal is to throw an exception as soon as possible, but
-- persistent is still transitioning to that.
--
-- SQL backends automatically create uniqueness constraints, but for MongoDB
-- you must manually place a unique index on a field to have a uniqueness
-- constraint.
--
class (PersistCore backend, PersistStoreRead backend) =>
      PersistUniqueRead backend  where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record -> ReaderT backend m (Maybe (Entity record))

-- | Some functions in this module ('insertUnique', 'insertBy', and
-- 'replaceUnique') first query the unique indexes to check for
-- conflicts. You could instead optimistically attempt to perform the
-- operation (e.g. 'replace' instead of 'replaceUnique'). However,
--
--  * there is some fragility to trying to catch the correct exception and
--  determing the column of failure;
--
--  * an exception will automatically abort the current SQL transaction.
class (PersistUniqueRead backend, PersistStoreWrite backend) =>
      PersistUniqueWrite backend  where
    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record -> ReaderT backend m ()
    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique
        :: (MonadIO m, PersistRecordBackend record backend)
        => record -> ReaderT backend m (Maybe (Key record))
    insertUnique datum = do
        conflict <- checkUnique datum
        case conflict of
            Nothing -> Just `liftM` insert datum
            Just _ -> return Nothing
    -- | Update based on a uniqueness constraint or insert:
    --
    -- * insert the new record if it does not exist;
    -- * If the record exists (matched via it's uniqueness constraint), then update the existing record with the parameters which is passed on as list to the function.
    --
    -- Throws an exception if there is more than 1 uniqueness constraint.
    upsert
        :: (MonadIO m, PersistRecordBackend record backend)
        => record          -- ^ new record to insert
        -> [Update record]  -- ^ updates to perform if the record already exists (leaving
                            -- this empty is the equivalent of performing a 'repsert' on a
                            -- unique key)
        -> ReaderT backend m (Entity record) -- ^ the record in the database after the operation
    upsert record updates = do
        uniqueKey <- onlyUnique record
        upsertBy uniqueKey record updates
    -- | Update based on a given uniqueness constraint or insert:
    --
    -- * insert the new record if it does not exist;
    -- * update the existing record that matches the given uniqueness constraint.
    upsertBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record   -- ^ uniqueness constraint to find by
        -> record          -- ^ new record to insert
        -> [Update record] -- ^ updates to perform if the record already exists (leaving
                           -- this empty is the equivalent of performing a 'repsert' on a
                           -- unique key)
        -> ReaderT backend m (Entity record) -- ^ the record in the database after the operation
    upsertBy uniqueKey record updates = do
        mrecord <- getBy uniqueKey
        maybe (insertEntity record) (`updateGetEntity` updates) mrecord
      where
        updateGetEntity (Entity k _) upds =
            (Entity k) `liftM` (updateGet k upds)

    -- | Put many records into db
    --
    -- * insert new records that do not exist (or violate any unique constraints)
    -- * replace existing records (matching any unique constraint)
    -- @since 2.8.1
    putMany
        :: (MonadIO m, PersistRecordBackend record backend)
        => [record]             -- ^ A list of the records you want to insert or replace.
        -> ReaderT backend m ()
    putMany = defaultPutMany

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
insertBy
    :: (MonadIO m
       ,PersistUniqueWrite backend
       ,PersistRecordBackend record backend)
    => record -> ReaderT backend m (Either (Entity record) (Key record))
insertBy val = do
    res <- getByValue val
    case res of
        Nothing -> Right `liftM` insert val
        Just z -> return $ Left z

-- | It is `repsert` that checking on a given uniqueness constraint.
-- if a record exist by unique, then `replace`, else `insert`.
-- `repsertBy` corresponds to `repsert` so that `upsertBy` corresponds to `upsert`.
-- This function differs from upsertBy only by specifying records as arguments,
-- so you do not have to arrange fields to update.
--
-- @since 2.8.3
repsertBy
    :: ( MonadIO m
       , PersistUniqueWrite backend
       , PersistRecordBackend record backend
       )
    => record                         -- ^ new record to `replace` or `insert`.
    -> ReaderT backend m (Key record) -- ^ new id by `insert` or id already present.
repsertBy record = do
    me <- getByValue record
    case me of
        Nothing             -> insert record
        Just (Entity key _) -> replace key record >> return key

-- | Insert a value, checking for conflicts with any unique constraints. If a
-- duplicate exists in the database, it is left untouched. The key of the
-- existing or new entry is returned
_insertOrGet :: (MonadIO m, PersistUniqueWrite backend, PersistRecordBackend record backend)
            => record -> ReaderT backend m (Key record)
_insertOrGet val = do
    res <- getByValue val
    case res of
        Nothing -> insert val
        Just (Entity key _) -> return key

-- | Like 'insertEntity', but returns 'Nothing' when the record
-- couldn't be inserted because of a uniqueness constraint.
--
-- @since 2.7.1
insertUniqueEntity
    :: (MonadIO m
       ,PersistRecordBackend record backend
       ,PersistUniqueWrite backend)
    => record -> ReaderT backend m (Maybe (Entity record))
insertUniqueEntity datum =
  fmap (\key -> Entity key datum) `liftM` insertUnique datum

-- | Return the single unique key for a record.
onlyUnique
    :: (MonadIO m
       ,PersistUniqueWrite backend
       ,PersistRecordBackend record backend)
    => record -> ReaderT backend m (Unique record)
onlyUnique record =
    case onlyUniqueEither record of
        Right u -> return u
        Left us ->
            requireUniques record us >>=
            liftIO . throwIO . OnlyUniqueException . show . length

onlyUniqueEither
    :: (PersistEntity record)
    => record -> Either [Unique record] (Unique record)
onlyUniqueEither record =
    case persistUniqueKeys record of
        [u] -> Right u
        us -> Left us

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' record. Returns a record matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue
    :: (MonadIO m
       ,PersistUniqueRead backend
       ,PersistRecordBackend record backend)
    => record -> ReaderT backend m (Maybe (Entity record))
getByValue record =
    checkUniques =<< requireUniques record (persistUniqueKeys record)
  where
    checkUniques [] = return Nothing
    checkUniques (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> checkUniques xs
            Just z -> return $ Just z

requireUniques
    :: (MonadIO m, PersistEntity record)
    => record -> [Unique record] -> m [Unique record]
requireUniques record [] = liftIO $ throwIO $ userError errorMsg
  where
    errorMsg = "getByValue: " `Data.Monoid.mappend` unpack (recordName record) `mappend` " does not have any Unique"

requireUniques _ xs = return xs

-- TODO: expose this to users
recordName
    :: (PersistEntity record)
    => record -> Text
recordName = unHaskellName . entityHaskell . entityDef . Just

-- | Attempt to replace the record of the given key with the given new record.
-- First query the unique fields to make sure the replacement maintains
-- uniqueness constraints.
--
-- Return 'Nothing' if the replacement was made.
-- If uniqueness is violated, return a 'Just' with the 'Unique' violation
--
-- @since 1.2.2.0
replaceUnique
    :: (MonadIO m
       ,Eq record
       ,Eq (Unique record)
       ,PersistRecordBackend record backend
       ,PersistUniqueWrite backend)
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
checkUnique
    :: (MonadIO m
       ,PersistRecordBackend record backend
       ,PersistUniqueRead backend)
    => record -> ReaderT backend m (Maybe (Unique record))
checkUnique = checkUniqueKeys . persistUniqueKeys

checkUniqueKeys
    :: (MonadIO m
       ,PersistEntity record
       ,PersistUniqueRead backend
       ,PersistRecordBackend record backend)
    => [Unique record] -> ReaderT backend m (Maybe (Unique record))
checkUniqueKeys [] = return Nothing
checkUniqueKeys (x:xs) = do
    y <- getBy x
    case y of
        Nothing -> checkUniqueKeys xs
        Just _ -> return (Just x)

-- | The slow but generic 'putMany' implemetation for any 'PersistUniqueRead'.
-- * Lookup corresponding entities (if any) for each record using 'getByValue'
-- * For pre-existing records, issue a 'replace' for each old key and new record
-- * For new records, issue a bulk 'insertMany_'
defaultPutMany
    ::( PersistEntityBackend record ~ BaseBackend backend
      , PersistEntity record
      , MonadIO m
      , PersistStoreWrite backend
      , PersistUniqueRead backend
      )
    => [record]
    -> ReaderT backend m ()
defaultPutMany []   = return ()
defaultPutMany rsD  = do
    let rs = nubBy ((==) `on` persistUniqueKeyValues) (reverse rsD)

    -- lookup record(s) by their unique key
    mEsOld <- mapM getByValue rs

    -- find pre-existing entities and corresponding (incoming) records
    let merge (Just x) y = Just (x, y)
        merge _        _ = Nothing
    let mEsOldAndRs = zipWith merge mEsOld rs
    let esOldAndRs = catMaybes mEsOldAndRs

    -- determine records to insert
    let esOld = fmap fst esOldAndRs
    let rsOld = fmap entityVal esOld
    let rsNew = deleteFirstsBy ((==) `on` persistUniqueKeyValues) rs rsOld

    -- determine records to update
    let rsUpd = fmap snd esOldAndRs
    let ksOld = fmap entityKey esOld
    let krs   = zip ksOld rsUpd

    -- insert `new` records
    insertMany_ rsNew
    -- replace existing records
    mapM_ (uncurry replace) krs

-- | The _essence_ of a unique record.
-- useful for comaparing records in haskell land for uniqueness equality.
persistUniqueKeyValues :: PersistEntity record => record -> [PersistValue]
persistUniqueKeyValues r = concat $ map persistUniqueToValues $ persistUniqueKeys r
