{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Class.PersistQuery
    ( PersistQueryRead (..)
    , PersistQueryWrite (..)
    , selectSource
    , selectKeys
    , selectList
    , selectKeysList
    ) where

import Database.Persist.Types
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (ReaderT, MonadReader)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity
import Control.Monad.Trans.Resource (MonadResource, release)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Acquire (Acquire, allocateAcquire, with)

-- | Backends supporting conditional read operations.
class (PersistCore backend, PersistStoreRead backend) => PersistQueryRead backend where
    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSourceRes
           :: (PersistRecordBackend record backend, MonadIO m1, MonadIO m2)
           => [Filter record]
           -> [SelectOpt record]
           -> ReaderT backend m1 (Acquire (C.Source m2 (Entity record)))

    -- | Get just the first record for the criterion.
    selectFirst :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record]
                -> [SelectOpt record]
                -> ReaderT backend m (Maybe (Entity record))
    selectFirst filts opts = do
        srcRes <- selectSourceRes filts (LimitTo 1 : opts)
        liftIO $ with srcRes (C.$$ CL.head)

    -- | Get the 'Key's of all records matching the given criterion.
    selectKeysRes
        :: (MonadIO m1, MonadIO m2, PersistRecordBackend record backend)
        => [Filter record]
        -> [SelectOpt record]
        -> ReaderT backend m1 (Acquire (C.Source m2 (Key record)))

    -- | The total number of records fulfilling the given criterion.
    count :: (MonadIO m, PersistRecordBackend record backend)
          => [Filter record] -> ReaderT backend m Int

-- | Backends supporting conditional write operations
class (PersistQueryRead backend, PersistStoreWrite backend) => PersistQueryWrite backend where
    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record backend)
                => [Filter record] -> [Update record] -> ReaderT backend m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record backend)
                => [Filter record] -> ReaderT backend m ()

-- | Get all records matching the given criterion in the specified order.
-- Returns also the identifiers.
selectSource
       :: (PersistQueryRead (BaseBackend backend), MonadResource m, PersistEntity record, PersistEntityBackend record ~ BaseBackend (BaseBackend backend), MonadReader backend m, HasPersistBackend backend)
       => [Filter record]
       -> [SelectOpt record]
       -> C.Source m (Entity record)
selectSource filts opts = do
    srcRes <- liftPersist $ selectSourceRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records matching the given criterion.
selectKeys :: (PersistQueryRead (BaseBackend backend), MonadResource m, PersistEntity record, BaseBackend (BaseBackend backend) ~ PersistEntityBackend record, MonadReader backend m, HasPersistBackend backend)
           => [Filter record]
           -> [SelectOpt record]
           -> C.Source m (Key record)
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
    liftIO $ with srcRes (C.$$ CL.consume)

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
               => [Filter record]
               -> [SelectOpt record]
               -> ReaderT backend m [Key record]
selectKeysList filts opts = do
    srcRes <- selectKeysRes filts opts
    liftIO $ with srcRes (C.$$ CL.consume)
