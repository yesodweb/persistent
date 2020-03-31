{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
module Database.Persist.Class.PersistStore
    ( HasPersistBackend (..)
    , IsPersistBackend (..)
    , PersistRecordBackend
    , liftPersist
    , PersistCore (..)
    , PersistStoreRead (..)
    , PersistStoreWrite (..)
    , getEntity
    , getJust
    , getJustEntity
    , belongsTo
    , belongsToJust
    , insertEntity
    , insertRecord
    , ToBackendKey(..)
    , BackendCompatible(..)
    ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistField
import Database.Persist.Types

-- | Class which allows the plucking of a @BaseBackend backend@ from some larger type.
-- For example,
-- @
-- instance HasPersistBackend (SqlReadBackend, Int) where
--   type BaseBackend (SqlReadBackend, Int) = SqlBackend
--   persistBackend = unSqlReadBackend . fst
-- @
class HasPersistBackend backend where
    type BaseBackend backend
    persistBackend :: backend -> BaseBackend backend
-- | Class which witnesses that @backend@ is essentially the same as @BaseBackend backend@.
-- That is, they're isomorphic and @backend@ is just some wrapper over @BaseBackend backend@.
class (HasPersistBackend backend) => IsPersistBackend backend where
    -- | This function is how we actually construct and tag a backend as having read or write capabilities.
    -- It should be used carefully and only when actually constructing a @backend@. Careless use allows us
    -- to accidentally run a write query against a read-only database.
    mkPersistBackend :: BaseBackend backend -> backend

-- | This class witnesses that two backend are compatible, and that you can
-- convert from the @sub@ backend into the @sup@ backend. This is similar
-- to the 'HasPersistBackend' and 'IsPersistBackend' classes, but where you
-- don't want to fix the type associated with the 'PersistEntityBackend' of
-- a record.
--
-- Generally speaking, where you might have:
--
-- @
-- foo ::
--   ( 'PersistEntity' record
--   , 'PeristEntityBackend' record ~ 'BaseBackend' backend
--   , 'IsSqlBackend' backend
--   )
-- @
--
-- this can be replaced with:
--
-- @
-- foo ::
--   ( 'PersistEntity' record,
--   , 'PersistEntityBackend' record ~ backend
--   , 'BackendCompatible' 'SqlBackend' backend
--   )
-- @
--
-- This works for 'SqlReadBackend' because of the @instance 'BackendCompatible' 'SqlBackend' 'SqlReadBackend'@, without needing to go through the 'BaseBackend' type family.
--
-- Likewise, functions that are currently hardcoded to use 'SqlBackend' can be generalized:
--
-- @
-- -- before:
-- asdf :: 'ReaderT' 'SqlBackend' m ()
-- asdf = pure ()
--
-- -- after:
-- asdf' :: 'BackendCompatible' SqlBackend backend => ReaderT backend m ()
-- asdf' = withReaderT 'projectBackend' asdf
-- @
--
-- @since 2.7.1
class BackendCompatible sup sub where
    projectBackend :: sub -> sup

-- | A convenient alias for common type signatures
type PersistRecordBackend record backend = (PersistEntity record, PersistEntityBackend record ~ BaseBackend backend)

liftPersist
    :: (MonadIO m, MonadReader backend m)
    => ReaderT backend IO b -> m b
liftPersist f = do
    env <- ask
    liftIO $ runReaderT f env

-- | 'ToBackendKey' converts a 'PersistEntity' 'Key' into a 'BackendKey'
-- This can be used by each backend to convert between a 'Key' and a plain
-- Haskell type. For Sql, that is done with 'toSqlKey' and 'fromSqlKey'.
--
-- By default, a 'PersistEntity' uses the default 'BackendKey' for its Key
-- and is an instance of ToBackendKey
--
-- A 'Key' that instead uses a custom type will not be an instance of
-- 'ToBackendKey'.
class ( PersistEntity record
      , PersistEntityBackend record ~ backend
      , PersistCore backend
      ) => ToBackendKey backend record where
    toBackendKey   :: Key record -> BackendKey backend
    fromBackendKey :: BackendKey backend -> Key record

class PersistCore backend where
    data BackendKey backend

class
  ( Show (BackendKey backend), Read (BackendKey backend)
  , Eq (BackendKey backend), Ord (BackendKey backend)
  , PersistCore backend
  , PersistField (BackendKey backend), A.ToJSON (BackendKey backend), A.FromJSON (BackendKey backend)
  ) => PersistStoreRead backend where
    -- | Get a record by identifier, if available.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > getSpj :: MonadIO m => ReaderT SqlBackend m (Maybe User)
    -- > getSpj = get spjId
    --
    -- > mspj <- getSpj
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will get this:
    --
    -- > +------+-----+
    -- > | name | age |
    -- > +------+-----+
    -- > | SPJ  |  40 |
    -- > +------+-----+
    get :: forall record m. (MonadIO m, PersistRecordBackend record backend)
        => Key record -> ReaderT backend m (Maybe record)

    -- | Get many records by their respective identifiers, if available.
    --
    -- @since 2.8.1
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>:
    --
    -- > getUsers :: MonadIO m => ReaderT SqlBackend m (Map (Key User) User)
    -- > getUsers = getMany allkeys
    --
    -- > musers <- getUsers
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will get these records:
    --
    -- > +----+-------+-----+
    -- > | id | name  | age |
    -- > +----+-------+-----+
    -- > |  1 | SPJ   |  40 |
    -- > +----+-------+-----+
    -- > |  2 | Simon |  41 |
    -- > +----+-------+-----+
    getMany
        :: forall record m. (MonadIO m, PersistRecordBackend record backend)
        => [Key record] -> ReaderT backend m (Map (Key record) record)
    getMany [] = return Map.empty
    getMany ks = do
        vs <- mapM get ks
        let kvs   = zip ks vs
        let kvs'  = (fmap Maybe.fromJust) `fmap` filter (\(_,v) -> Maybe.isJust v) kvs
        return $ Map.fromList kvs'

class
  ( Show (BackendKey backend), Read (BackendKey backend)
  , Eq (BackendKey backend), Ord (BackendKey backend)
  , PersistStoreRead backend
  , PersistField (BackendKey backend), A.ToJSON (BackendKey backend), A.FromJSON (BackendKey backend)
  ) => PersistStoreWrite backend where

    -- | Create a new record in the database, returning an automatically created
    -- key (in SQL an auto-increment id).
    --
    -- === __Example usage__
    --
    -- Using <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>, let's insert a new user 'John'.
    --
    -- > insertJohn :: MonadIO m => ReaderT SqlBackend m (Key User)
    -- > insertJohn = insert $ User "John" 30
    --
    -- > johnId <- insertJohn
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |John  |30   |
    -- > +-----+------+-----+
    insert :: forall record m. (MonadIO m, PersistRecordBackend record backend)
           => record -> ReaderT backend m (Key record)

    -- | Same as 'insert', but doesn't return a @Key@.
    --
    -- === __Example usage__
    --
    -- with <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > insertJohn :: MonadIO m => ReaderT SqlBackend m (Key User)
    -- > insertJohn = insert_ $ User "John" 30
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |John  |30   |
    -- > +-----+------+-----+
    insert_ :: forall record m. (MonadIO m, PersistRecordBackend record backend)
            => record -> ReaderT backend m ()
    insert_ record = insert record >> return ()

    -- | Create multiple records in the database and return their 'Key's.
    --
    -- If you don't need the inserted 'Key's, use 'insertMany_'.
    --
    -- The MongoDB and PostgreSQL backends insert all records and
    -- retrieve their keys in one database query.
    --
    -- The SQLite and MySQL backends use the slow, default implementation of
    -- @mapM insert@.
    --
    -- === __Example usage__
    --
    -- with <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > insertUsers :: MonadIO m => ReaderT SqlBackend m [Key User]
    -- > insertUsers = insertMany [User "John" 30, User "Nick" 32, User "Jane" 20]
    --
    -- > userIds <- insertUsers
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |John  |30   |
    -- > +-----+------+-----+
    -- > |4    |Nick  |32   |
    -- > +-----+------+-----+
    -- > |5    |Jane  |20   |
    -- > +-----+------+-----+
    insertMany :: forall record m. (MonadIO m, PersistRecordBackend record backend)
               => [record] -> ReaderT backend m [Key record]
    insertMany = mapM insert

    -- | Same as 'insertMany', but doesn't return any 'Key's.
    --
    -- The MongoDB, PostgreSQL, SQLite and MySQL backends insert all records in
    -- one database query.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > insertUsers_ :: MonadIO m => ReaderT SqlBackend m ()
    -- > insertUsers_ = insertMany_ [User "John" 30, User "Nick" 32, User "Jane" 20]
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |John  |30   |
    -- > +-----+------+-----+
    -- > |4    |Nick  |32   |
    -- > +-----+------+-----+
    -- > |5    |Jane  |20   |
    -- > +-----+------+-----+
    insertMany_ :: forall record m. (MonadIO m, PersistRecordBackend record backend)
                => [record] -> ReaderT backend m ()
    insertMany_ x = insertMany x >> return ()

    -- | Same as 'insertMany_', but takes an 'Entity' instead of just a record.
    --
    -- Useful when migrating data from one entity to another
    -- and want to preserve ids.
    --
    -- The MongoDB, PostgreSQL, SQLite and MySQL backends insert all records in
    -- one database query.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > insertUserEntityMany :: MonadIO m => ReaderT SqlBackend m ()
    -- > insertUserEntityMany = insertEntityMany [SnakeEntity, EvaEntity]
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |Snake |38   |
    -- > +-----+------+-----+
    -- > |4    |Eva   |38   |
    -- > +-----+------+-----+
    insertEntityMany :: forall record m. (MonadIO m, PersistRecordBackend record backend)
                     => [Entity record] -> ReaderT backend m ()
    insertEntityMany = mapM_ (\(Entity k record) -> insertKey k record)

    -- | Create a new record in the database using the given key.
    --
    -- === __Example usage__
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > insertAliceKey :: MonadIO m => Key User -> ReaderT SqlBackend m ()
    -- > insertAliceKey key = insertKey key $ User "Alice" 20
    --
    -- > insertAliceKey $ UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 3}}
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |Alice |20   |
    -- > +-----+------+-----+
    insertKey :: forall record m. (MonadIO m, PersistRecordBackend record backend)
              => Key record -> record -> ReaderT backend m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    --
    -- === __Example usage__
    --
    -- We try to explain 'upsertBy' using <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>.
    --
    -- First, we insert Philip to <#dataset-persist-store-1 dataset-1>.
    --
    -- > insertPhilip :: MonadIO m => ReaderT SqlBackend m (Key User)
    -- > insertPhilip = insert $ User "Philip" 42
    --
    -- > philipId <- insertPhilip
    --
    -- This query will produce:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |Philip|42   |
    -- > +-----+------+-----+
    --
    -- > repsertHaskell :: MonadIO m => Key record -> ReaderT SqlBackend m ()
    -- > repsertHaskell id = repsert id $ User "Haskell" 81
    --
    -- > repsertHaskell philipId
    --
    -- This query will replace Philip's record with Haskell's one:
    --
    -- > +-----+-----------------+--------+
    -- > |id   |name             |age     |
    -- > +-----+-----------------+--------+
    -- > |1    |SPJ              |40      |
    -- > +-----+-----------------+--------+
    -- > |2    |Simon            |41      |
    -- > +-----+-----------------+--------+
    -- > |3    |Philip -> Haskell|42 -> 81|
    -- > +-----+-----------------+--------+
    --
    -- 'repsert' inserts the given record if the key doesn't exist.
    --
    -- > repsertXToUnknown :: MonadIO m => ReaderT SqlBackend m ()
    -- > repsertXToUnknown = repsert unknownId $ User "X" 999
    --
    -- For example, applying the above query to <#dataset-persist-store-1 dataset-1> will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |40   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    -- > |3    |X     |999  |
    -- > +-----+------+-----+
    repsert :: forall record m. (MonadIO m, PersistRecordBackend record backend)
            => Key record -> record -> ReaderT backend m ()

    -- | Put many entities into the database.
    --
    -- Batch version of 'repsert' for SQL backends.
    --
    -- Useful when migrating data from one entity to another
    -- and want to preserve ids.
    --
    -- @since 2.8.1
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > repsertManyUsers :: MonadIO m =>ReaderT SqlBackend m ()
    -- > repsertManyusers = repsertMany [(simonId, User "Philip" 20), (unknownId999, User "Mr. X" 999)]
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+----------------+---------+
    -- > |id   |name            |age      |
    -- > +-----+----------------+---------+
    -- > |1    |SPJ             |40       |
    -- > +-----+----------------+---------+
    -- > |2    |Simon -> Philip |41 -> 20 |
    -- > +-----+----------------+---------+
    -- > |999  |Mr. X           |999      |
    -- > +-----+----------------+---------+
    repsertMany
        :: forall record m. (MonadIO m, PersistRecordBackend record backend)
        => [(Key record, record)] -> ReaderT backend m ()
    repsertMany = mapM_ (uncurry repsert)

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey' or 'repsert' in
    -- these cases.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1 schama-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > replaceSpj :: MonadIO m => User -> ReaderT SqlBackend m ()
    -- > replaceSpj record = replace spjId record
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |Mike  |45   |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    replace :: forall record m. (MonadIO m, PersistRecordBackend record backend)
            => Key record -> record -> ReaderT backend m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > deleteSpj :: MonadIO m => ReaderT SqlBackend m ()
    -- > deleteSpj = delete spjId
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    delete :: forall record m. (MonadIO m, PersistRecordBackend record backend)
           => Key record -> ReaderT backend m ()

    -- | Update individual fields on a specific record.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > updateSpj :: MonadIO m => [Update User] -> ReaderT SqlBackend m ()
    -- > updateSpj updates = update spjId updates
    --
    -- > updateSpj [UserAge +=. 100]
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |140  |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    update :: forall record m. (MonadIO m, PersistRecordBackend record backend)
           => Key record -> [Update record] -> ReaderT backend m ()

    -- | Update individual fields on a specific record, and retrieve the
    -- updated value from the database.
    --
    -- Note that this function will throw an exception if the given key is not
    -- found in the database.
    --
    -- === __Example usage__
    --
    -- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
    --
    -- > updateGetSpj :: MonadIO m => [Update User] -> ReaderT SqlBackend m User
    -- > updateGetSpj updates = updateGet spjId updates
    --
    -- > spj <- updateGetSpj [UserAge +=. 100]
    --
    -- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
    --
    -- > +-----+------+-----+
    -- > |id   |name  |age  |
    -- > +-----+------+-----+
    -- > |1    |SPJ   |140  |
    -- > +-----+------+-----+
    -- > |2    |Simon |41   |
    -- > +-----+------+-----+
    updateGet :: forall record m. (MonadIO m, PersistRecordBackend record backend)
              => Key record -> [Update record] -> ReaderT backend m record
    updateGet key ups = do
        update key ups
        get key >>= maybe (liftIO $ throwIO $ KeyNotFound $ show key) return


-- | Same as 'get', but for a non-null (not Maybe) foreign key.
-- Unsafe unless your database is enforcing that the foreign key is valid.
--
-- === __Example usage__
--
-- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
--
-- > getJustSpj :: MonadIO m => ReaderT SqlBackend m User
-- > getJustSpj = getJust spjId
--
-- > spj <- getJust spjId
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will get this record:
--
-- > +----+------+-----+
-- > | id | name | age |
-- > +----+------+-----+
-- > |  1 | SPJ  |  40 |
-- > +----+------+-----+
--
-- > getJustUnknown :: MonadIO m => ReaderT SqlBackend m User
-- > getJustUnknown = getJust unknownId
--
-- mrx <- getJustUnknown
--
-- This just throws an error.
getJust :: forall record backend m.
  ( PersistStoreRead backend
  , PersistRecordBackend record backend
  , MonadIO m)
  => Key record -> ReaderT backend m record
getJust key = get key >>= maybe
  (liftIO $ throwIO $ PersistForeignConstraintUnmet $ T.pack $ show key)
  return

-- | Same as 'getJust', but returns an 'Entity' instead of just the record.
--
-- @since 2.6.1
--
-- === __Example usage__
--
-- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
--
-- > getJustEntitySpj :: MonadIO m => ReaderT SqlBackend m (Entity User)
-- > getJustEntitySpj = getJustEntity spjId
--
-- > spjEnt <- getJustEntitySpj
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will get this entity:
--
-- > +----+------+-----+
-- > | id | name | age |
-- > +----+------+-----+
-- > |  1 | SPJ  |  40 |
-- > +----+------+-----+
getJustEntity :: forall record backend m.
  ( PersistEntityBackend record ~ BaseBackend backend
  , MonadIO m
  , PersistEntity record
  , PersistStoreRead backend)
  => Key record -> ReaderT backend m (Entity record)
getJustEntity key = do
  record <- getJust key
  return $
    Entity
    { entityKey = key
    , entityVal = record
    }

-- | Curry this to make a convenience function that loads an associated model.
--
-- > foreign = belongsTo foreignId
belongsTo :: forall ent1 ent2 backend m.
  ( PersistStoreRead backend
  , PersistEntity ent1
  , PersistRecordBackend ent2 backend
  , MonadIO m
  ) => (ent1 -> Maybe (Key ent2)) -> ent1 -> ReaderT backend m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | Same as 'belongsTo', but uses @getJust@ and therefore is similarly unsafe.
belongsToJust :: forall ent1 ent2 backend m.
  ( PersistStoreRead backend
  , PersistEntity ent1
  , PersistRecordBackend ent2 backend
  , MonadIO m
  )
  => (ent1 -> Key ent2) -> ent1 -> ReaderT backend m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Like @insert@, but returns the complete @Entity@.
--
-- === __Example usage__
--
-- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
--
-- > insertHaskellEntity :: MonadIO m => ReaderT SqlBackend m (Entity User)
-- > insertHaskellEntity = insertEntity $ User "Haskell" 81
--
-- > haskellEnt <- insertHaskellEntity
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
--
-- > +----+---------+-----+
-- > | id |  name   | age |
-- > +----+---------+-----+
-- > |  1 | SPJ     |  40 |
-- > +----+---------+-----+
-- > |  2 | Simon   |  41 |
-- > +----+---------+-----+
-- > |  3 | Haskell |  81 |
-- > +----+---------+-----+
insertEntity :: forall e backend m.
    ( PersistStoreWrite backend
    , PersistRecordBackend e backend
    , MonadIO m
    ) => e -> ReaderT backend m (Entity e)
insertEntity e = do
    eid <- insert e
    return $ Entity eid e

-- | Like @get@, but returns the complete @Entity@.
--
-- === __Example usage__
--
-- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
--
-- > getSpjEntity :: MonadIO m => ReaderT SqlBackend m (Maybe (Entity User))
-- > getSpjEntity = getEntity spjId
--
-- > mSpjEnt <- getSpjEntity
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will get this entity:
--
-- > +----+------+-----+
-- > | id | name | age |
-- > +----+------+-----+
-- > |  1 | SPJ  |  40 |
-- > +----+------+-----+
getEntity :: forall e backend m.
    ( PersistStoreRead backend
    , PersistRecordBackend e backend
    , MonadIO m
    ) => Key e -> ReaderT backend m (Maybe (Entity e))
getEntity key = do
    maybeModel <- get key
    return $ fmap (key `Entity`) maybeModel

-- | Like 'insertEntity' but just returns the record instead of 'Entity'.
--
-- @since 2.6.1
--
-- === __Example usage__
--
-- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>,
--
-- > insertDaveRecord :: MonadIO m => ReaderT SqlBackend m User
-- > insertDaveRecord = insertRecord $ User "Dave" 50
--
-- > dave <- insertDaveRecord
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will produce this:
--
-- > +-----+------+-----+
-- > |id   |name  |age  |
-- > +-----+------+-----+
-- > |1    |SPJ   |40   |
-- > +-----+------+-----+
-- > |2    |Simon |41   |
-- > +-----+------+-----+
-- > |3    |Dave  |50   |
-- > +-----+------+-----+
insertRecord
  :: forall record backend m. (PersistEntityBackend record ~ BaseBackend backend
     ,PersistEntity record
     ,MonadIO m
     ,PersistStoreWrite backend)
  => record -> ReaderT backend m record
insertRecord record = do
  insert_ record
  return $ record
