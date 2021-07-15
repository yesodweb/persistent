{-# LANGUAGE ExplicitForAll #-}
module Database.Persist.Class.PersistQuery
    ( PersistQueryRead (..)
    , PersistQueryWrite (..)
    , PersistQueryStream (..)
    , selectSource
    , selectKeys
    , selectList
    , selectKeysList
    , selectStream
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(ask), ReaderT, lift)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire, allocateAcquire, with)
import Data.Conduit (ConduitM, await, runConduit, (.|))
import Data.Conduit.Lift (runReaderC)
import qualified Data.Conduit.List as CL

import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistStore

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

    -- | Check if there is at least one record fulfilling the given criterion.
    --
    -- @since 2.11
    exists :: (MonadIO m, PersistRecordBackend record backend)
           => [Filter record] -> ReaderT backend m Bool

-- | Backends supporting conditional read operations, which may also support
-- streaming the results in a memory-constant way.
--
-- @since 2.13.2.0
class (PersistQueryRead backend) => PersistQueryStream backend where
    -- | Get all records matching the given criterion in the specified order.
    --
    -- A version of 'selectSourceRes' which specifically streams the results,
    -- for SQL backends that support it. Streaming may be slower for small
    -- query sets, but avoids allocating all the results in memory at once.
    --
    -- By default, this behaves identically to 'selectSourceRes'.
    --
    -- @since 2.13.2.0
    selectSourceStream
        :: (PersistRecordBackend record backend, MonadResource m)
        => [Filter record]
        -> [SelectOpt record]
        -> ConduitM () (Entity record) (ReaderT backend m) ()
    selectSourceStream filts opts = do
      srcRes <- lift $ selectSourceRes filts opts
      (releaseKey, src) <- allocateAcquire srcRes
      src
      release releaseKey

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
       :: forall record backend m. (PersistQueryRead backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
       => [Filter record]
       -> [SelectOpt record]
       -> ConduitM () (Entity record) m ()
selectSource filts opts = do
    srcRes <- liftPersist $ selectSourceRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records matching the given criterion.
selectKeys :: forall record backend m. (PersistQueryRead backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
           => [Filter record]
           -> [SelectOpt record]
           -> ConduitM () (Key record) m ()
selectKeys filts opts = do
    srcRes <- liftPersist $ selectKeysRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Call 'selectSource' but return the result as a list.
selectList :: forall record backend m. (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
           => [Filter record]
           -> [SelectOpt record]
           -> ReaderT backend m [Entity record]
selectList filts opts = do
    srcRes <- selectSourceRes filts opts
    liftIO $ with srcRes (\src -> runConduit $ src .| CL.consume)

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: forall record backend m. (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
               => [Filter record]
               -> [SelectOpt record]
               -> ReaderT backend m [Key record]
selectKeysList filts opts = do
    srcRes <- selectKeysRes filts opts
    liftIO $ with srcRes (\src -> runConduit $ src .| CL.consume)

-- | Get all records matching the given criterion in the specified order. Uses
-- streaming where possible, based on the 'PersistQueryStream' instance for the
-- backend.
--
-- @since 2.13.2.0
selectStream :: forall record backend m. (PersistQueryStream backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
             => [Filter record]
             -> [SelectOpt record]
             -> ConduitM () (Entity record) m ()
selectStream filts opts = lift ask >>= (`runReaderC` selectSourceStream filts opts)
