{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.Persist.Class.PersistUnique
  ( PersistUniqueRead(..)
  , PersistUniqueWrite(..)
  , OnlyOneUniqueKey(..)
  , onlyOneUniqueDef
  , AtLeastOneUniqueKey(..)
  , atLeastOneUniqueDef
  , NoUniqueKeysError
  , MultipleUniqueKeysError
  , getByValue
  , getByValueUniques
  , insertBy
  , insertUniqueEntity
  , replaceUnique
  , checkUnique
  , onlyUnique
  , defaultPutMany
  , persistUniqueKeyValues
  )
  where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Function (on)
import Data.List ((\\), deleteFirstsBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage(..))

import Database.Persist.Types
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity

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
        :: forall record m. (MonadIO m, PersistRecordBackend record backend)
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
        :: forall record m. (MonadIO m, PersistRecordBackend record backend)
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
        :: forall record m. (MonadIO m, PersistRecordBackend record backend)
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
    -- > upsertX updates = upsert (User "X" 999) updates
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
    -- This fails with a compile-time type error alerting us to the fact
    -- that this record has multiple unique keys, and suggests that we look for
    -- 'upsertBy' to select the unique key we want.
    upsert
        :: forall record m. (MonadIO m, PersistRecordBackend record backend, OnlyOneUniqueKey record)
        => record
        -- ^ new record to insert
        -> [Update record]
        -- ^ updates to perform if the record already exists
        -> ReaderT backend m (Entity record)
        -- ^ the record in the database after the operation
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
        :: forall record m. (MonadIO m, PersistRecordBackend record backend)
        => Unique record
        -- ^ uniqueness constraint to find by
        -> record
        -- ^ new record to insert
        -> [Update record]
        -- ^ updates to perform if the record already exists
        -> ReaderT backend m (Entity record)
        -- ^ the record in the database after the operation
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
    --
    -- @since 2.8.1
    putMany
        :: forall record m.
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
class PersistEntity record => OnlyOneUniqueKey record where
    onlyUniqueP :: record -> Unique record

-- | Given a proxy for a 'PersistEntity' record, this returns the sole
-- 'UniqueDef' for that entity.
--
-- @since 2.10.0
onlyOneUniqueDef
    :: (OnlyOneUniqueKey record, Monad proxy)
    => proxy record
    -> UniqueDef
onlyOneUniqueDef prxy =
    case entityUniques (entityDef prxy) of
        [uniq] -> uniq
        _ -> error "impossible due to OnlyOneUniqueKey constraint"

-- | This is an error message. It is used when writing instances of
-- 'OnlyOneUniqueKey' for an entity that has no unique keys.
--
-- @since 2.10.0
type NoUniqueKeysError ty =
    'Text "The entity "
        ':<>: 'ShowType ty
        ':<>: 'Text " does not have any unique keys."
    ':$$: 'Text "The function you are trying to call requires a unique key "
        ':<>: 'Text "to be defined on the entity."

-- | This is an error message. It is used when an entity has multiple
-- unique keys, and the function expects a single unique key.
--
-- @since 2.10.0
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
class PersistEntity record => AtLeastOneUniqueKey record where
    requireUniquesP :: record -> NonEmpty (Unique record)

-- | Given a proxy for a record that has an instance of
-- 'AtLeastOneUniqueKey', this returns a 'NonEmpty' list of the
-- 'UniqueDef's for that entity.
--
-- @since 2.10.0
atLeastOneUniqueDef
    :: (AtLeastOneUniqueKey record, Monad proxy)
    => proxy record
    -> NonEmpty UniqueDef
atLeastOneUniqueDef prxy =
    case entityUniques (entityDef prxy) of
        (x:xs) -> x :| xs
        _ ->
            error "impossible due to AtLeastOneUniqueKey record constraint"

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
    :: forall record backend m.
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
    :: forall record backend m. (MonadIO m
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
-- @mSimonConst@ would be Simon's uniqueness constraint. Note that
-- @onlyUnique@ doesn't work if there're more than two constraints. It will
-- fail with a type error instead.
onlyUnique
    :: forall record backend m.
    ( MonadIO m
    , PersistUniqueWrite backend
    , PersistRecordBackend record backend
    , OnlyOneUniqueKey record
    )
    => record -> ReaderT backend m (Unique record)
onlyUnique = pure . onlyUniqueP

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
    let uniqs = requireUniquesP record
    getByValueUniques (NEL.toList uniqs)

-- | Retrieve a record from the database using the given unique keys. It
-- will attempt to find a matching record for each 'Unique' in the list,
-- and returns the first one that has a match.
--
-- Returns 'Nothing' if you provide an empty list ('[]') or if no value
-- matches in the database.
--
-- @since 2.10.0
getByValueUniques
    :: forall record backend m.
    ( MonadIO m
    , PersistUniqueRead backend
    , PersistRecordBackend record backend
    )
    => [Unique record]
    -> ReaderT backend m (Maybe (Entity record))
getByValueUniques uniqs =
    checkUniques uniqs
  where
    checkUniques [] = return Nothing
    checkUniques (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> checkUniques xs
            Just z -> return $ Just z

-- | Attempt to replace the record of the given key with the given new record.
-- First query the unique fields to make sure the replacement maintains
-- uniqueness constraints.
--
-- Return 'Nothing' if the replacement was made.
-- If uniqueness is violated, return a 'Just' with the 'Unique' violation
--
-- @since 1.2.2.0
replaceUnique
    :: forall record backend m. ( MonadIO m
       , Eq (Unique record)
       , PersistRecordBackend record backend
       , PersistUniqueWrite backend )
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
    :: forall record backend m. ( MonadIO m
       , PersistRecordBackend record backend
       , PersistUniqueRead backend)
    => record -> ReaderT backend m (Maybe (Unique record))
checkUnique = checkUniqueKeys . persistUniqueKeys

checkUniqueKeys
    :: forall record backend m. ( MonadIO m
       , PersistUniqueRead backend
       , PersistRecordBackend record backend)
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
    :: forall record backend m. ( PersistEntityBackend record ~ BaseBackend backend
      , PersistEntity record
      , MonadIO m
      , PersistStoreWrite backend
      , PersistUniqueRead backend
      )
    => [record]
    -> ReaderT backend m ()
defaultPutMany []   = return ()
defaultPutMany rsD@(e:_)  = do
    case persistUniqueKeys e of
        [] -> insertMany_ rsD
        _ -> go
  where
    go = do
        -- deduplicate the list of records in Haskell by unique key. The
        -- previous implementation used Data.List.nubBy which is O(n^2)
        -- complexity.
        let rs = map snd
                . Map.toList
                . Map.fromList
                . map (\r -> (persistUniqueKeyValues r, r))
                $ rsD

        -- lookup record(s) by their unique key
        mEsOld <- mapM (getByValueUniques . persistUniqueKeys) rs

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

-- | This function returns a list of 'PersistValue' that correspond to the
-- 'Unique' keys on that record. This is useful for comparing two @record@s
-- for equality only on the basis of their 'Unique' keys.
persistUniqueKeyValues :: PersistEntity record => record -> [PersistValue]
persistUniqueKeyValues = concatMap persistUniqueToValues . persistUniqueKeys
