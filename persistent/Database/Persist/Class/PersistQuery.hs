{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.Class.PersistQuery
    ( PersistQuery (..)
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
                => [Filter val] -> [Update val] -> m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
                => [Filter val] -> ReaderT backend m ()

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSource -- FIXME need a better API for this and selectKeys
           :: (PersistEntity val, PersistEntityBackend val ~ backend)
           => [Filter val]
           -> [SelectOpt val]
           -> C.Source m (Entity val)

    -- | get just the first record for the criterion
    selectFirst :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
                => [Filter val]
                -> [SelectOpt val]
                -> m (Maybe (Entity val))
    selectFirst filts opts = selectSource filts ((LimitTo 1):opts) C.$$ CL.head


    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
               => [Filter val]
               -> [SelectOpt val]
               -> C.Source m (Key val)

    -- | The total number of records fulfilling the given criterion.
    count :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val)
          => [Filter val] -> m Int

-- | Call 'selectSource' but return the result as a list.
selectList :: (MonadIO m, PersistEntity val, PersistQuery backend, PersistEntityBackend val ~ backend)
           => [Filter val]
           -> [SelectOpt val]
           -> m [Entity val]
selectList a b = selectSource a b C.$$ CL.consume

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (MonadIO m, PersistEntity val, PersistQuery backend, PersistEntityBackend val ~ backend)
               => [Filter val]
               -> [SelectOpt val]
               -> m [Key val]
selectKeysList a b = selectKeys a b C.$$ CL.consume
