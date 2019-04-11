{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE TypeOperators, DataKinds #-}

module Database.Persist.Class.PersistUnique
  ( PersistUniqueRead(..)
  , PersistUniqueWrite(..)
  , OnlyOneUniqueKey(..)
  , AtLeastOneUniqueKey(..)
  , NoUniqueKeysError
  , MultipleUniqueKeysError
  , getByValue
  , insertBy
  , insertUniqueEntity
  , replaceUnique
  , checkUnique
  , onlyUnique
  , defaultPutMany
  , persistUniqueKeyValues
  )
  where

import Data.List.NonEmpty (NonEmpty)
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
import GHC.TypeLits

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
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-unique-1 schema-1> and <#dataset-persist-unique-1 dataset-1>:
    --
    -- > getBySpjName :: MonadIO m  => ReaderT SqlBackend m (Maybe (Entity User))
    -- > getBySpjName = getBy $ UniqueUserName "SPJ"
    --
    -- > mSpjEnt <- getBySpjName
    --
    -- The above query when applied on <#dataset-persist-unique-1 dataset-1>, will get this entity:
    --
    -- > +----+------+-----+
    -- > | id | name | age |
    -- > +----+------+-----+
    -- > |  1 | SPJ  |  40 |
    -- > +----+------+-----+
    getBy
        ::
        ( MonadIO m
        , PersistRecordBackend record backend
        )
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
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-unique-1 schema-1> and <#dataset-persist-unique-1 dataset-1>,
    --
    -- > deleteBySpjName :: MonadIO m => ReaderT SqlBackend m ()
    -- > deleteBySpjName = deleteBy UniqueUserName "SPJ"
    --
    -- The above query when applied on <#dataset-persist-unique-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    deleteBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record -> ReaderT backend m ()
    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-unique-1 schema-1> and <#dataset-persist-unique-1 dataset-1>, we try to insert the following two records:
    --
    -- > linusId <- insertUnique $ User "Linus" 48
    -- > spjId   <- insertUnique $ User "SPJ" 90
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |Linus |48   |
    -- > +-----+------+-----+
    --
    -- Linus's record was inserted to <#dataset-persist-unique-1 dataset-1>, while SPJ wasn't because SPJ already exists in <#dataset-persist-unique-1 dataset-1>.
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
    --
    -- === __Example usage__
    --
    -- First, we try to explain 'upsert' using <#schema-persist-unique-1 schema-1> and <#dataset-persist-unique-1 dataset-1>.
    --
    -- > upsertSpj :: MonadIO m => [Update User] -> ReaderT SqlBackend m (Maybe (Entity User))
    -- > upsertSpj updates = upsert (User "SPJ" 999) upadtes
    --
    -- > mSpjEnt <- upsertSpj [UserAge +=. 15]
    --
    -- The above query when applied on <#dataset-persist-unique-1 dataset-1>, will produce this:
    --
    -- > +-----+-----+--------+
    -- > |id   |name |age     |
    -- > +-----+-----+--------+
    -- > |1    |SPJ  |40 -> 55|
    -- > +-----+-----+--------+
    -- > |2    |Simon|41      |
    -- > +-----+-----+--------+
    --
    -- > upsertX :: MonadIO m => [Update User] -> ReaderT SqlBackend m (Maybe (Entity User))
    -- > upsertX updates = upsert (User "X" 999) upadtes
    --
    -- > mXEnt <- upsertX [UserAge +=. 15]
    --
    -- The above query when applied on <#dataset-persist-unique-1 dataset-1>, will produce this:
    --
    -- > +-----+-----+--------+
    -- > |id   |name |age     |
    -- > +-----+-----+--------+
    -- > |1    |SPJ  |40      |
    -- > +-----+-----+--------+
    -- > |2    |Simon|41      |
    -- > +-----+-----+--------+
    -- > |3    |X    |999     |
    -- > +-----+-----+--------+
    --
    -- Next, what if the schema has two uniqueness constraints?
    -- Let's check it out using <#schema-persist-unique-2 schema-2>:
    --
    -- > mSpjEnt <- upsertSpj [UserAge +=. 15]
    --
    -- Then, it throws an error message something like "Expected only one unique key, got"
    upsert
        :: (MonadIO m, PersistRecordBackend record backend, OnlyOneUniqueKey record)
        => record          -- ^ new record to insert
        -> [Update record]  -- ^ updates to perform if the record already exists
        -> ReaderT backend m (Entity record) -- ^ the record in the database after the operation
    upsert record updates = do
        uniqueKey <- onlyUnique record
        upsertBy uniqueKey record updates
    -- | Update based on a given uniqueness constraint or insert:
    --
    -- * insert the new record if it does not exist;
    -- * update the existing record that matches the given uniqueness constraint.
    --
    -- === __Example usage__
    --
    -- We try to explain 'upsertBy' using <#schema-persist-unique-2 schema-2> and <#dataset-persist-unique-1 dataset-1>.
    --
    -- > upsertBySpjName :: MonadIO m => User -> [Update User] -> ReaderT SqlBackend m (Entity User)
    -- > upsertBySpjName record updates = upsertBy (UniqueUserName "SPJ") record updates
    --
    -- > mSpjEnt <- upsertBySpjName (Person "X" 999) [PersonAge += .15]
    --
    -- The above query will alter <#dataset-persist-unique-1 dataset-1> to:
    --
    -- > +-----+-----+--------+
    -- > |id   |name |age     |
    -- > +-----+-----+--------+
    -- > |1    |SPJ  |40 -> 55|
    -- > +-----+-----+--------+
    -- > |2    |Simon|41      |
    -- > +-----+-----+--------+
    --
    -- > upsertBySimonAge :: MonadIO m => User -> [Update User] -> ReaderT SqlBackend m (Entity User)
    -- > upsertBySimonAge record updates = upsertBy (UniqueUserName "SPJ") record updates
    --
    -- > mPhilipEnt <- upsertBySimonAge (User "X" 999) [UserName =. "Philip"]
    --
    -- The above query will alter <#dataset-persist-unique-1 dataset-1> to:
    --
    -- > +----+-----------------+-----+
    -- > | id |      name       | age |
    -- > +----+-----------------+-----+
    -- > |  1 | SPJ             |  40 |
    -- > +----+-----------------+-----+
    -- > |  2 | Simon -> Philip |  41 |
    -- > +----+-----------------+-----+
    --
    -- > upsertByUnknownName :: MonadIO m => User -> [Update User] -> ReaderT SqlBackend m (Entity User)
    -- > upsertByUnknownName record updates = upsertBy (UniqueUserName "Unknown") record updates
    --
    -- > mXEnt <- upsertByUnknownName (User "X" 999) [UserAge +=. 15]
    --
    -- This query will alter <#dataset-persist-unique-1 dataset-1> to:
    --
    -- > +-----+-----+-----+
    -- > |id   |name |age  |
    -- > +-----+-----+-----+
    -- > |1    |SPJ  |40   |
    -- > +-----+-----+-----+
    -- > |2    |Simon|41   |
    -- > +-----+-----+-----+
    -- > |3    |X    |999  |
    -- > +-----+-----+-----+
    upsertBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record   -- ^ uniqueness constraint to find by
        -> record          -- ^ new record to insert
        -> [Update record] -- ^ updates to perform if the record already exists
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
        ::
        ( MonadIO m
        , PersistRecordBackend record backend
        )
        => [record]
        -- ^ A list of the records you want to insert or replace.
        -> ReaderT backend m ()
    putMany = defaultPutMany

-- | This class is used to ensure that 'upsert' is only called on records
-- that have a single 'Unique' key. The quasiquoter automatically generates
-- working instances for appropriate records, and generates 'TypeError'
-- instances for records that have 0 or multiple unique keys.
--
-- @since 2.10.0
class OnlyOneUniqueKey record where
    onlyUniqueP :: proxy record -> Unique record

type NoUniqueKeysError ty =
    'Text "The entity "
        ':<>: 'ShowType ty
        ':<>: 'Text " does not have any unique keys."
    ':$$: 'Text "The function you are trying to call requires a unique key "
        ':<>: 'Text "to be defined on the entity."

type MultipleUniqueKeysError ty =
    'Text "The entity "
        ':<>: 'ShowType ty
        ':<>: 'Text " has multiple unique keys."
    ':$$: 'Text "The function you are trying to call requires only a single "
        ':<>: 'Text "unique key."
    ':$$: 'Text "There is probably a variant of the function with 'By' "
        ':<>: 'Text "appended that will allow you to select a unique key "
        ':<>: 'Text "for the operation."

-- | This class is used to ensure that functions requring at least one
-- unique key are not called with records that have 0 unique keys. The
-- quasiquoter automatically writes working instances for appropriate
-- entities, and generates 'TypeError' instances for records that have
-- 0 unique keys.
--
-- @since 2.10.0
class AtLeastOneUniqueKey record where
    requireUniquesP :: proxy record -> NonEmpty (Unique record)

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
--
-- === __Example usage__
--
-- With <#schema-persist-unique-2 schema-2> and <#dataset-persist-unique-1 dataset-1>, we have following lines of code:
--
-- > l1 <- insertBy $ User "SPJ" 20
-- > l2 <- insertBy $ User "XXX" 41
-- > l3 <- insertBy $ User "SPJ" 40
-- > r1 <- insertBy $ User "XXX" 100
--
-- First three lines return 'Left' because there're duplicates in given record's uniqueness constraints. While the last line returns a new key as 'Right'.
insertBy
    ::
    ( MonadIO m
    , PersistUniqueWrite backend
    , PersistRecordBackend record backend
    , AtLeastOneUniqueKey record
    )
    => record -> ReaderT backend m (Either (Entity record) (Key record))
insertBy val = do
    res <- getByValue val
    case res of
        Nothing -> Right `liftM` insert val
        Just z -> return $ Left z

-- | Insert a value, checking for conflicts with any unique constraints. If a
-- duplicate exists in the database, it is left untouched. The key of the
-- existing or new entry is returned
_insertOrGet
    ::
    ( MonadIO m
    , PersistUniqueWrite backend
    , PersistRecordBackend record backend
    , AtLeastOneUniqueKey record
    )
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
--
-- === __Example usage__
--
-- We use <#schema-persist-unique-2 schema-2> and <#dataset-persist-unique-1 dataset-1> here.
--
-- > insertUniqueSpjEntity :: MonadIO m => ReaderT SqlBackend m (Maybe (Entity User))
-- > insertUniqueSpjEntity = insertUniqueEntity $ User "SPJ" 50
--
-- > mSpjEnt <- insertUniqueSpjEntity
--
-- The above query results 'Nothing' as SPJ already exists.
--
-- > insertUniqueAlexaEntity :: MonadIO m => ReaderT SqlBackend m (Maybe (Entity User))
-- > insertUniqueAlexaEntity = insertUniqueEntity $ User "Alexa" 3
--
-- > mAlexaEnt <- insertUniqueSpjEntity
--
-- Because there's no such unique keywords of the given record, the above query when applied on <#dataset-persist-unique-1 dataset-1>, will produce this:
--
-- > +----+-------+-----+
-- > | id | name  | age |
-- > +----+-------+-----+
-- > |  1 | SPJ   |  40 |
-- > +----+-------+-----+
-- > |  2 | Simon |  41 |
-- > +----+-------+-----+
-- > |  3 | Alexa |   3 |
-- > +----+-------+-----+

insertUniqueEntity
    :: (MonadIO m
       ,PersistRecordBackend record backend
       ,PersistUniqueWrite backend)
    => record -> ReaderT backend m (Maybe (Entity record))
insertUniqueEntity datum =
  fmap (\key -> Entity key datum) `liftM` insertUnique datum

-- | Return the single unique key for a record.
--
-- === __Example usage__
--
-- We use shcema-1 and <#dataset-persist-unique-1 dataset-1> here.
--
-- > onlySimonConst :: MonadIO m => ReaderT SqlBackend m (Unique User)
-- > onlySimonConst = onlyUnique $ User "Simon" 999
--
-- > mSimonConst <- onlySimonConst
--
-- @mSimonConst@ would be Simon's uniqueness constraint. Note that @onlyUnique@ doesn't work if there're more than two constraints.
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
--
-- === __Example usage__
--
-- With <#schema-persist-unique-1 schema-1> and <#dataset-persist-unique-1 dataset-1>,
--
-- getBySpjValue :: MonadIO m => ReaderT SqlBackend m (Maybe (Entity User))
-- getBySpjValue = getByValue $ User "SPJ" 999
--
-- > mSpjEnt <- getBySpjValue
--
-- The above query when applied on <#dataset-persist-unique-1 dataset-1>, will get this record:
--
-- > +----+------+-----+
-- > | id | name | age |
-- > +----+------+-----+
-- > |  1 | SPJ  |  40 |
-- > +----+------+-----+
getByValue
    :: forall record m backend.
    ( MonadIO m
    , PersistUniqueRead backend
    , PersistRecordBackend record backend
    , AtLeastOneUniqueKey record
    )
    => record -> ReaderT backend m (Maybe (Entity record))
getByValue record = do
    uniqs <- requireUniques record (persistUniqueKeys record)
    getByValueUniques uniqs record

-- | Retrieve a record from the database using the given unique keys.
--
-- @since 2.10.0
getByValueUniques
    ::
    ( MonadIO m
    , PersistUniqueRead backend
    , PersistRecordBackend record backend
    )
    => [Unique record]
    -> record
    -> ReaderT backend m (Maybe (Entity record))
getByValueUniques uniqs _ =
    checkUniques uniqs
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
--
-- === __Example usage__
--
-- We use <#schema-persist-unique-1 schema-1> and <#dataset-persist-unique-1 dataset-1> here.
--
-- This would be 'Nothing':
--
-- > mAlanConst <- checkUnique $ User "Alan" 70
--
-- While this would be 'Just' because SPJ already exists:
--
-- > mSpjConst <- checkUnique $ User "SPJ" 60
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
    let uKeys = persistUniqueKeys . head $ rsD
    case uKeys of
        [] -> insertMany_ rsD
        uniqs -> go uniqs
  where
    go uniqs = do
        let rs = nubBy ((==) `on` persistUniqueKeyValues) (reverse rsD)

        -- lookup record(s) by their unique key
        mEsOld <- mapM (getByValueUniques uniqs) rs

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
