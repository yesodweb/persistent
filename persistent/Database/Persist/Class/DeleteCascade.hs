{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.Class.DeleteCascade
    ( DeleteCascade (..)
    , deleteCascadeWhere
    ) where

import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistQuery
import Database.Persist.Class.PersistEntity

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (with)

class (PersistStore backend, PersistEntity a, backend ~ PersistEntityBackend a) => DeleteCascade a backend where
    deleteCascade :: MonadIO m => Key a -> ReaderT backend m ()

deleteCascadeWhere :: (MonadIO m, DeleteCascade a backend, PersistQuery backend)
                   => [Filter a] -> ReaderT backend m ()
deleteCascadeWhere filts = do
    srcRes <- selectKeysRes filts []
    conn <- ask
    liftIO $ with srcRes (C.$$ CL.mapM_ (flip runReaderT conn . deleteCascade))
