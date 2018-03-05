{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
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

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistField
import Database.Persist.Types
import qualified Data.Aeson as A
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe

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
    :: (MonadIO m, MonadReader backend m, HasPersistBackend backend)
    => ReaderT (BaseBackend backend) IO b -> m b
liftPersist f = do
    env <- ask
    liftIO $ runReaderT f (persistBackend env)

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
    get :: (MonadIO m, PersistRecordBackend record backend)
        => Key record -> ReaderT backend m (Maybe record)

    -- | Get many records by their respective identifiers, if available.
    -- @since 2.8.1
    getMany
        :: (MonadIO m, PersistRecordBackend record backend)
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
    insert :: (MonadIO m, PersistRecordBackend record backend)
           => record -> ReaderT backend m (Key record)

    -- | Same as 'insert', but doesn't return a @Key@.
    insert_ :: (MonadIO m, PersistRecordBackend record backend)
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
    insertMany :: (MonadIO m, PersistRecordBackend record backend)
               => [record] -> ReaderT backend m [Key record]
    insertMany = mapM insert

    -- | Same as 'insertMany', but doesn't return any 'Key's.
    --
    -- The MongoDB, PostgreSQL, SQLite and MySQL backends insert all records in
    -- one database query.
    insertMany_ :: (MonadIO m, PersistRecordBackend record backend)
                => [record] -> ReaderT backend m ()
    insertMany_ x = insertMany x >> return ()

    -- | Same as 'insertMany_', but takes an 'Entity' instead of just a record.
    --
    -- Useful when migrating data from one entity to another
    -- and want to preserve ids.
    --
    -- The MongoDB, PostgreSQL, SQLite and MySQL backends insert all records in
    -- one database query.
    insertEntityMany :: (MonadIO m, PersistRecordBackend record backend)
                     => [Entity record] -> ReaderT backend m ()
    insertEntityMany = mapM_ (\(Entity k record) -> insertKey k record)

    -- | Create a new record in the database using the given key.
    insertKey :: (MonadIO m, PersistRecordBackend record backend)
              => Key record -> record -> ReaderT backend m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    --
    -- === __Example usage__
    --
    -- First, we insert Philip:
    --
    -- @
    -- philipId <- insert $ User \"Philip\" $ Just 42
    -- @
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
    -- @
    -- repsert philipId (User \"Haskell\" 55)
    -- @
    --
    -- The dataset is now:
    --
    -- > +-----+-----------------+--------+
    -- > |id   |name             |age     |
    -- > +-----+-----------------+--------+
    -- > |1    |SPJ              |40      |
    -- > +-----+-----------------+--------+
    -- > |2    |Simon            |41      |
    -- > +-----+-----------------+--------+
    -- > |3    |Philip -> Haskell|42 -> 55|
    -- > +-----+-----------------+--------+
    --
    -- 'repsert' inserts the given record if the key
    -- doesn't exist. For example, with the first dataset, the following code will insert the user X
    --
    -- @
    -- repsert unknownId  (User \"X\" 999)
    -- @
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
    repsert :: (MonadIO m, PersistRecordBackend record backend)
            => Key record -> record -> ReaderT backend m ()

    -- | Put many entities into the database.
    --
    -- Batch version of 'repsert' for SQL backends.
    --
    -- Useful when migrating data from one entity to another
    -- and want to preserve ids.
    --
    -- Differs from @insertEntityMany@ by gracefully skipping
    -- pre-existing records matching key(s).
    -- @since 2.8.1
    repsertMany
        :: (MonadIO m, PersistRecordBackend record backend)
        => [(Key record, record)] -> ReaderT backend m ()
    repsertMany = mapM_ (uncurry repsert)

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey or 'repsert' in
    -- these cases.
    replace :: (MonadIO m, PersistRecordBackend record backend)
            => Key record -> record -> ReaderT backend m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: (MonadIO m, PersistRecordBackend record backend)
           => Key record -> ReaderT backend m ()

    -- | Update individual fields on a specific record.
    update :: (MonadIO m, PersistRecordBackend record backend)
           => Key record -> [Update record] -> ReaderT backend m ()

    -- | Update individual fields on a specific record, and retrieve the
    -- updated value from the database.
    --
    -- Note that this function will throw an exception if the given key is not
    -- found in the database.
    updateGet :: (MonadIO m, PersistRecordBackend record backend)
              => Key record -> [Update record] -> ReaderT backend m record
    updateGet key ups = do
        update key ups
        get key >>= maybe (liftIO $ throwIO $ KeyNotFound $ show key) return


-- | Same as 'get', but for a non-null (not Maybe) foreign key.
-- Unsafe unless your database is enforcing that the foreign key is valid.
getJust :: ( PersistStoreRead backend
           , Show (Key record)
           , PersistRecordBackend record backend
           , MonadIO m
           ) => Key record -> ReaderT backend m record
getJust key = get key >>= maybe
  (liftIO $ throwIO $ PersistForeignConstraintUnmet $ T.pack $ show key)
  return

-- | Same as 'getJust', but returns an 'Entity' instead of just the record.
--
-- @since 2.6.1
getJustEntity
  :: (PersistEntityBackend record ~ BaseBackend backend
     ,MonadIO m
     ,PersistEntity record
     ,PersistStoreRead backend)
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
belongsTo ::
  ( PersistStoreRead backend
  , PersistEntity ent1
  , PersistRecordBackend ent2 backend
  , MonadIO m
  ) => (ent1 -> Maybe (Key ent2)) -> ent1 -> ReaderT backend m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | Same as 'belongsTo', but uses @getJust@ and therefore is similarly unsafe.
belongsToJust ::
  ( PersistStoreRead backend
  , PersistEntity ent1
  , PersistRecordBackend ent2 backend
  , MonadIO m
  )
  => (ent1 -> Key ent2) -> ent1 -> ReaderT backend m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Like @insert@, but returns the complete @Entity@.
insertEntity ::
    ( PersistStoreWrite backend
    , PersistRecordBackend e backend
    , MonadIO m
    ) => e -> ReaderT backend m (Entity e)
insertEntity e = do
    eid <- insert e
    return $ Entity eid e

-- | Like @get@, but returns the complete @Entity@.
getEntity ::
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
insertRecord
  :: (PersistEntityBackend record ~ BaseBackend backend
     ,PersistEntity record
     ,MonadIO m
     ,PersistStoreWrite backend)
  => record -> ReaderT backend m record
insertRecord record = do
  insert_ record
  return $ record
