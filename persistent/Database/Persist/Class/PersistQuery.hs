{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.Class.PersistQuery
    ( PersistQuery (..)
    , selectSource
    , selectKeys
    , selectList
    , selectKeysList
    ) where

import Control.Exception (throwIO)
import Database.Persist.Types


import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.Reader   ( ReaderT  )

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (Resource, MonadResource, allocateResource, release, with)

class PersistStore backend => PersistQuery backend where
    -- | Update individual fields on a specific record.
    update :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
           => Key val -> [Update val] -> ReaderT backend m ()

    -- | Update individual fields on a specific record, and retrieve the
    -- updated value from the database.
    --
    -- Note that this function will throw an exception if the given key is not
    -- found in the database.
    updateGet :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
              => Key val -> [Update val] -> ReaderT backend m val
    updateGet key ups = do
        update key ups
        get key >>= maybe (liftIO $ throwIO $ KeyNotFound $ show key) return

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
                => [Filter val] -> [Update val] -> ReaderT backend m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
                => [Filter val] -> ReaderT backend m ()

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSourceRes
           :: (PersistEntity val, PersistEntityBackend val ~ backend, MonadIO m1, MonadIO m2)
           => [Filter val]
           -> [SelectOpt val]
           -> ReaderT backend m1 (Resource (C.Source m2 (Entity val)))

    -- | get just the first record for the criterion
    selectFirst :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
                => [Filter val]
                -> [SelectOpt val]
                -> ReaderT backend m (Maybe (Entity val))
    selectFirst filts opts = do
        srcRes <- selectSourceRes filts ((LimitTo 1):opts)
        liftIO $ with srcRes (C.$$ CL.head)

    -- | Get the 'Key's of all records matching the given criterion.
    selectKeysRes
        :: (MonadIO m1, MonadIO m2, PersistEntity val, backend ~ PersistEntityBackend val)
        => [Filter val]
        -> [SelectOpt val]
        -> ReaderT backend m1 (Resource (C.Source m2 (Key val)))

    -- | The total number of records fulfilling the given criterion.
    count :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
          => [Filter val] -> ReaderT backend m Int

-- | Get all records matching the given criterion in the specified order.
-- Returns also the identifiers.
selectSource
       :: (PersistQuery backend, MonadResource m, PersistEntity val, PersistEntityBackend val ~ backend)
       => [Filter val]
       -> [SelectOpt val]
       -> C.Source (ReaderT backend m) (Entity val)
selectSource filts opts = do
    srcRes <- lift $ selectSourceRes filts opts
    (releaseKey, src) <- allocateResource srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records matching the given criterion.
selectKeys :: (PersistQuery backend, MonadResource m, PersistEntity val, backend ~ PersistEntityBackend val)
           => [Filter val]
           -> [SelectOpt val]
           -> C.Source (ReaderT backend m) (Key val)
selectKeys filts opts = do
    srcRes <- lift $ selectKeysRes filts opts
    (releaseKey, src) <- allocateResource srcRes
    src
    release releaseKey

-- | Call 'selectSource' but return the result as a list.
selectList :: (MonadIO m, PersistEntity val, PersistQuery backend, PersistEntityBackend val ~ backend)
           => [Filter val]
           -> [SelectOpt val]
           -> ReaderT backend m [Entity val]
selectList filts opts = do
    srcRes <- selectSourceRes filts opts
    liftIO $ with srcRes (C.$$ CL.consume)

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (MonadIO m, PersistEntity val, PersistQuery backend, PersistEntityBackend val ~ backend)
               => [Filter val]
               -> [SelectOpt val]
               -> ReaderT backend m [Key val]
selectKeysList filts opts = do
    srcRes <- selectKeysRes filts opts
    liftIO $ with srcRes (C.$$ CL.consume)
