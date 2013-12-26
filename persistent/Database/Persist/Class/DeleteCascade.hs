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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)

class (PersistStore backend, PersistEntity a, backend ~ PersistEntityBackend a) => DeleteCascade a backend where
    deleteCascade :: MonadIO m => Key a -> ReaderT backend m ()

deleteCascadeWhere :: (MonadIO m, DeleteCascade a backend, PersistQuery backend)
                   => [Filter a] -> ReaderT backend m ()
deleteCascadeWhere filts = selectKeys filts [] C.$$ CL.mapM_ deleteCascade
