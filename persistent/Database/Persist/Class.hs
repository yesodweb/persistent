{-# LANGUAGE ConstraintKinds #-}

module Database.Persist.Class
    ( ToBackendKey (..)

    -- * PersistStore
    , PersistCore (..)
    , PersistStore
    , PersistStoreRead (..)
    , PersistStoreWrite (..)
    , BaseBackend(..)
    , PersistRecordBackend
    , getJust
    , belongsTo
    , belongsToJust
    , insertEntity

    -- * PersistUnique
    , PersistUnique
    , PersistUniqueRead (..)
    , PersistUniqueWrite (..)
    , getByValue
    , insertBy
    , replaceUnique
    , checkUnique
    , onlyUnique

    -- * PersistQuery
    , PersistQuery
    , PersistQueryRead (..)
    , PersistQueryWrite (..)
    , selectSource
    , selectKeys
    , selectList
    , selectKeysList

    -- * DeleteCascade
    , DeleteCascade (..)
    , deleteCascadeWhere

    -- * PersistEntity
    , PersistEntity (..)
    -- * PersistField
    , PersistField (..)
    -- * PersistConfig
    , PersistConfig (..)
    , entityValues

    -- * Lifting
    , HasPersistBackend (..)
    , IsPersistBackend ()
    , liftPersist

    -- * JSON utilities
    , keyValueEntityToJSON, keyValueEntityFromJSON
    , entityIdToJSON, entityIdFromJSON
    , toPersistValueJSON, fromPersistValueJSON
    ) where

import Database.Persist.Class.DeleteCascade
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistQuery
import Database.Persist.Class.PersistUnique
import Database.Persist.Class.PersistConfig
import Database.Persist.Class.PersistField
import Database.Persist.Class.PersistStore


-- | A backwards-compatible alias for those that don't care about distinguishing between read and write queries.
-- It signifies the assumption that, by default, a backend can write as well as read.
type PersistUnique a = PersistUniqueWrite a

-- | A backwards-compatible alias for those that don't care about distinguishing between read and write queries.
-- It signifies the assumption that, by default, a backend can write as well as read.
type PersistQuery a = PersistQueryWrite a

-- | A backwards-compatible alias for those that don't care about distinguishing between read and write queries.
-- It signifies the assumption that, by default, a backend can write as well as read.
type PersistStore a = PersistStoreWrite a
