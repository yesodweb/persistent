{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.Class.DeleteCascade
    ( DeleteCascade (..)
    ) where

import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity

class (PersistStore m, PersistEntity a, PersistEntityBackend a ~ PersistMonadBackend m) => DeleteCascade a m where
    deleteCascade :: Key a -> m ()
