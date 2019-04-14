{-# LANGUAGE ConstraintKinds #-}
module Database.Persist.Class
    ( ToBackendKey (..)

    -- * PersistStore
    -- |
    --
    -- All the examples present here will be explained based on these schemas, datasets and functions:
    --
    -- = schema-1
    --
    -- #schema-persist-store-1#
    --
    -- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    -- > User
    -- >     name String
    -- >     age Int
    -- >     deriving Show
    -- > |]
    --
    -- = dataset-1
    --
    -- #dataset-persist-store-1#
    --
    -- > +----+-------+-----+
    -- > | id | name  | age |
    -- > +----+-------+-----+
    -- > |  1 | SPJ   |  40 |
    -- > +----+-------+-----+
    -- > |  2 | Simon |  41 |
    -- > +----+-------+-----+

    , PersistCore (..)
    , PersistStore
    , PersistStoreRead (..)
    , PersistStoreWrite (..)
    , PersistRecordBackend
    , getJust
    , getJustEntity
    , getEntity
    , belongsTo
    , belongsToJust
    , insertEntity
    , insertRecord

    -- * PersistUnique
    -- |
    --
    -- All the examples present here will be explained based on these two schemas and the dataset:
    --
    -- = schema-1
    -- This schema has single unique constraint.
    --
    -- #schema-persist-unique-1#
    --
    -- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    -- > User
    -- >     name String
    -- >     age Int
    -- >     UniqueUserName name
    -- >     deriving Show
    -- > |]
    --
    -- = schema-2
    -- This schema has two unique constraints.
    --
    -- #schema-persist-unique-2#
    --
    -- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    -- > User
    -- >     name String
    -- >     age Int
    -- >     UniqueUserName name
    -- >     UniqueUserAge age
    -- >     deriving Show
    -- > |]
    --
    -- = dataset-1
    --
    -- #dataset-persist-unique-1#
    --
    -- > +-----+-----+-----+
    -- > |id   |name |age  |
    -- > +-----+-----+-----+
    -- > |1    |SPJ  |40   |
    -- > +-----+-----+-----+
    -- > |2    |Simon|41   |
    -- > +-----+-----+-----+

    , PersistUnique
    , PersistUniqueRead (..)
    , PersistUniqueWrite (..)
    , OnlyOneUniqueKey (..)
    , AtLeastOneUniqueKey (..)
    , NoUniqueKeysError
    , MultipleUniqueKeysError
    , getByValue
    , insertBy
    , insertUniqueEntity
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
    , BackendCompatible (..)

    -- * JSON utilities
    , keyValueEntityToJSON, keyValueEntityFromJSON
    , entityIdToJSON, entityIdFromJSON
    , toPersistValueJSON, fromPersistValueJSON
    ) where

import Database.Persist.Class.DeleteCascade
import Database.Persist.Class.PersistConfig
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistField
import Database.Persist.Class.PersistQuery
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistUnique


-- | A backwards-compatible alias for those that don't care about distinguishing between read and write queries.
-- It signifies the assumption that, by default, a backend can write as well as read.
type PersistUnique a = PersistUniqueWrite a

-- | A backwards-compatible alias for those that don't care about distinguishing between read and write queries.
-- It signifies the assumption that, by default, a backend can write as well as read.
type PersistQuery a = PersistQueryWrite a

-- | A backwards-compatible alias for those that don't care about distinguishing between read and write queries.
-- It signifies the assumption that, by default, a backend can write as well as read.
type PersistStore a = PersistStoreWrite a
