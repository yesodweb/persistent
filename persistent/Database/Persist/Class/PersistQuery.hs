module Database.Persist.Class.PersistQuery
    ( PersistQueryRead (..)
    , PersistQueryWrite (..)
    , selectSource
    , selectKeys
    , selectList
    , selectKeysList
    , selectFirstOrInsert
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (ReaderT, MonadReader)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire, allocateAcquire, with)
import Data.Conduit (ConduitM, (.|), await, runConduit)
import qualified Data.Conduit.List as CL

import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity

-- | Backends supporting conditional read operations.
class (PersistCore backend, PersistStoreRead backend) => PersistQueryRead backend where
    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSourceRes
           :: (PersistRecordBackend record backend, MonadIO m1, MonadIO m2)
           => [Filter record]
           -> [SelectOpt record]
           -> ReaderT backend m1 (Acquire (ConduitM () (Entity record) m2 ()))

    -- | Get just the first record for the criterion.
    selectFirst :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record]
                -> [SelectOpt record]
                -> ReaderT backend m (Maybe (Entity record))
    selectFirst filts opts = do
        srcRes <- selectSourceRes filts (LimitTo 1 : opts)
        liftIO $ with srcRes (\src -> runConduit $ src .| await)

    -- | Get the 'Key's of all records matching the given criterion.
    selectKeysRes
        :: (MonadIO m1, MonadIO m2, PersistRecordBackend record backend)
        => [Filter record]
        -> [SelectOpt record]
        -> ReaderT backend m1 (Acquire (ConduitM () (Key record) m2 ()))

    -- | The total number of records fulfilling the given criterion.
    count :: (MonadIO m, PersistRecordBackend record backend)
          => [Filter record] -> ReaderT backend m Int

-- | Backends supporting conditional write operations
class (PersistQueryRead backend, PersistStoreWrite backend) => PersistQueryWrite backend where
    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record] -> [Update record] -> ReaderT backend m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record] -> ReaderT backend m ()

-- | Get all records matching the given criterion in the specified order.
-- Returns also the identifiers.
selectSource
       :: (PersistQueryRead backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
       => [Filter record]
       -> [SelectOpt record]
       -> ConduitM () (Entity record) m ()
selectSource filts opts = do
    srcRes <- liftPersist $ selectSourceRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records matching the given criterion.
selectKeys :: (PersistQueryRead backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
           => [Filter record]
           -> [SelectOpt record]
           -> ConduitM () (Key record) m ()
selectKeys filts opts = do
    srcRes <- liftPersist $ selectKeysRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Call 'selectSource' but return the result as a list.
selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
           => [Filter record]
           -> [SelectOpt record]
           -> ReaderT backend m [Entity record]
selectList filts opts = do
    srcRes <- selectSourceRes filts opts
    liftIO $ with srcRes (\src -> runConduit $ src .| CL.consume)

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
               => [Filter record]
               -> [SelectOpt record]
               -> ReaderT backend m [Key record]
selectKeysList filts opts = do
    srcRes <- selectKeysRes filts opts
    liftIO $ with srcRes (\src -> runConduit $ src .| CL.consume)

-- | Get the first record matching the search criteria or create a new one if
-- there is no match.
--
-- @since 2.10.2
--
-- === __Example usage__
--
-- > personByName :: MonadIO m => Person -> ReaderT SqlBackend m (Entity Person)
-- > personByName Person{..}@rec = selectFirstOrInsert [PersonName ==. personName] [] rec
--
-- With <#schema-persist-store-1 schema-1> and <#dataset-persist-store-1 dataset-1>:
--
-- __When the record exists__
--
-- > personByName $ Person "SPJ" 40
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will
-- get these records:
--
-- > +----+-------+-----+
-- > | id | name  | age |
-- > +----+-------+-----+
-- > |  1 | SPJ   |  40 |
-- > +----+-------+-----+
--
-- __When the record does not exist__
--
-- > personByName $ Person "John" 42
--
-- The above query when applied on <#dataset-persist-store-1 dataset-1>, will
-- get these records:
--
-- > +----+-------+-----+
-- > | id | name  | age |
-- > +----+-------+-----+
-- > |  3 | John  |  42 |
-- > +----+-------+-----+
selectFirstOrInsert
    :: ( MonadIO m
       , PersistQueryRead backend
       , PersistRecordBackend record backend
       , PersistStoreWrite backend
       )
    => [Filter record]     -- ^ select filters
    -> [SelectOpt record]  -- ^ select options
    -> record              -- ^ record to be created if there is no match
    -> ReaderT backend m (Entity record)
selectFirstOrInsert filts opts rec = do
    mrec <- selectFirst filts opts
    maybe (insertEntity rec) return mrec
