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
import Data.Acquire (with)

class (PersistStore backend, PersistEntity record, backend ~ PersistEntityBackend record)
  => DeleteCascade record backend where
    deleteCascade :: MonadIO m => Key record -> ReaderT backend m ()

deleteCascadeWhere :: (MonadIO m, DeleteCascade record backend, PersistQuery backend)
                   => [Filter record] -> ReaderT backend m ()
deleteCascadeWhere filts = do
    srcRes <- selectKeysRes filts []
    conn <- ask
    liftIO $ with srcRes (C.$$ CL.mapM_ (flip runReaderT conn . deleteCascade))
