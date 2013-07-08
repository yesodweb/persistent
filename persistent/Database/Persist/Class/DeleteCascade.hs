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

class (PersistStore m, PersistEntity a, PersistEntityBackend a ~ PersistMonadBackend m) => DeleteCascade a m where
    deleteCascade :: Key a -> m ()

deleteCascadeWhere :: (DeleteCascade a m, PersistQuery m)
                   => [Filter a] -> m ()
deleteCascadeWhere filts = selectKeys filts [] C.$$ CL.mapM_ deleteCascade
