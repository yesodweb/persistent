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

class (PersistStore m, PersistEntity record, EntityBackend record ~ MonadBackend m) => DeleteCascade record m where
    deleteCascade :: Key record -> m ()

deleteCascadeWhere :: (DeleteCascade record m, PersistQuery m)
                   => [Filter record] -> m ()
deleteCascadeWhere filts = selectKeys filts [] C.$$ CL.mapM_ deleteCascade
