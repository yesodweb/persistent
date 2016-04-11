{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
module Database.Persist.Class.PersistStore
    ( HasPersistBackend (..)
    , IsPersistBackend (..)
    , liftPersist
    , PersistCore (..)
    , PersistStoreRead (..)
    , PersistStoreWrite (..)
    , getJust
    , belongsTo
    , belongsToJust
    , insertEntity
    , ToBackendKey(..)
    ) where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Lifted (throwIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistField
import Database.Persist.Types
import qualified Data.Aeson as A

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
    get :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
        => Key val -> ReaderT backend m (Maybe val)

class
  ( Show (BackendKey backend), Read (BackendKey backend)
  , Eq (BackendKey backend), Ord (BackendKey backend)
  , PersistStoreRead backend
  , PersistField (BackendKey backend), A.ToJSON (BackendKey backend), A.FromJSON (BackendKey backend)
  ) => PersistStoreWrite backend where

    -- | Create a new record in the database, returning an automatically created
    -- key (in SQL an auto-increment id).
    insert :: (MonadIO m, (BaseBackend backend) ~ PersistEntityBackend val, PersistEntity val)
           => val -> ReaderT backend m (Key val)

    -- | Same as 'insert', but doesn't return a @Key@.
    insert_ :: (MonadIO m, (BaseBackend backend) ~ PersistEntityBackend val, PersistEntity val)
            => val -> ReaderT backend m ()
    insert_ val = insert val >> return ()

    -- | Create multiple records in the database and return their 'Key's.
    --
    -- If you don't need the inserted 'Key's, use 'insertMany_'.
    --
    -- The MongoDB and PostgreSQL backends insert all records and
    -- retrieve their keys in one database query.
    --
    -- The SQLite and MySQL backends use the slow, default implementation of
    -- @mapM insert@.
    insertMany :: (MonadIO m, (BaseBackend backend) ~ PersistEntityBackend val, PersistEntity val)
               => [val] -> ReaderT backend m [Key val]
    insertMany = mapM insert

    -- | Same as 'insertMany', but doesn't return any 'Key's.
    --
    -- The MongoDB, PostgreSQL, SQLite and MySQL backends insert all records in
    -- one database query.
    insertMany_ :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
                => [val] -> ReaderT backend m ()
    insertMany_ x = insertMany x >> return ()

    -- | Same as 'insertMany_', but takes an 'Entity' instead of just a record.
    --
    -- Useful when migrating data from one entity to another
    -- and want to preserve ids.
    --
    -- The MongoDB backend inserts all the entities in one database query.
    --
    -- The SQL backends use the slow, default implementation of
    -- @mapM_ insertKey@.
    insertEntityMany :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
                     => [Entity val] -> ReaderT backend m ()
    insertEntityMany = mapM_ (\(Entity k record) -> insertKey k record)

    -- | Create a new record in the database using the given key.
    insertKey :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
              => Key val -> val -> ReaderT backend m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    repsert :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
            => Key val -> val -> ReaderT backend m ()

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey or 'repsert' in
    -- these cases.
    replace :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
            => Key val -> val -> ReaderT backend m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: (MonadIO m, BaseBackend backend ~ PersistEntityBackend val, PersistEntity val)
           => Key val -> ReaderT backend m ()

    -- | Update individual fields on a specific record.
    update :: (MonadIO m, PersistEntity val, BaseBackend backend ~ PersistEntityBackend val)
           => Key val -> [Update val] -> ReaderT backend m ()

    -- | Update individual fields on a specific record, and retrieve the
    -- updated value from the database.
    --
    -- Note that this function will throw an exception if the given key is not
    -- found in the database.
    updateGet :: (MonadIO m, PersistEntity val, BaseBackend backend ~ PersistEntityBackend val)
              => Key val -> [Update val] -> ReaderT backend m val
    updateGet key ups = do
        update key ups
        get key >>= maybe (liftIO $ throwIO $ KeyNotFound $ show key) return


-- | Same as get, but for a non-null (not Maybe) foreign key
-- Unsafe unless your database is enforcing that the foreign key is valid.
getJust :: ( PersistStoreRead backend
           , PersistEntity val
           , Show (Key val)
           , BaseBackend backend ~ PersistEntityBackend val
           , MonadIO m
           ) => Key val -> ReaderT backend m val
getJust key = get key >>= maybe
  (liftIO $ throwIO $ PersistForeignConstraintUnmet $ T.pack $ show key)
  return

-- | Curry this to make a convenience function that loads an associated model.
--
-- > foreign = belongsTo foreignId
belongsTo ::
  ( PersistStoreRead backend
  , PersistEntity ent1
  , PersistEntity ent2
  , BaseBackend backend ~ PersistEntityBackend ent2
  , MonadIO m
  ) => (ent1 -> Maybe (Key ent2)) -> ent1 -> ReaderT backend m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | Same as 'belongsTo', but uses @getJust@ and therefore is similarly unsafe.
belongsToJust ::
  ( PersistStoreRead backend
  , PersistEntity ent1
  , PersistEntity ent2
  , BaseBackend backend ~ PersistEntityBackend ent2
  , MonadIO m
  )
  => (ent1 -> Key ent2) -> ent1 -> ReaderT backend m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Like @insert@, but returns the complete @Entity@.
insertEntity ::
    ( PersistStoreWrite backend
    , PersistEntity e
    , BaseBackend backend ~ PersistEntityBackend e
    , MonadIO m
    ) => e -> ReaderT backend m (Entity e)
insertEntity e = do
    eid <- insert e
    return $ Entity eid e
