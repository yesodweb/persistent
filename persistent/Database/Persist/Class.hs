{-# LANGUAGE ConstraintKinds #-}

-- | This module exports all of the type classes in @persistent@ for operating
-- on the database backends.
--
-- @persistent@ offers methods that are abstract in the specific @backend@ type.
-- For SQL databases, this wil be 'Database.Persist.SqlBackend.SqlBackend'.
-- Other database backends will define their own types.
--
-- Methods and functions in this module have examples documented under an
-- "Example Usage" thing, that you need to click on to expand.
--
module Database.Persist.Class
    (

    -- * PersistStore
    -- | The 'PersistStore', 'PersistStoreRead', and 'PersistStoreWrite' type
    -- classes are used to define basic operations on the database. A database
    -- that implements these classes is capable of being used as a simple
    -- key-value store.
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
      PersistStore
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
    -- | The 'PersistUnique' type class is relevant for database backends that
    -- offer uniqueness keys. Uniquenes keys allow us to perform operations like
    -- 'getBy', 'deleteBy', as well as 'upsert' and 'putMany'.
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
    , onlyOneUniqueDef
    , NoUniqueKeysError
    , MultipleUniqueKeysError
    , getByValue
    , insertBy
    , insertUniqueEntity
    , replaceUnique
    , checkUnique
    , checkUniqueUpdateable
    , onlyUnique

    -- * PersistQuery
    -- |  The 'PersistQuery' type class allows us to select lists and filter
    -- database models.  'selectList' is the canonical read operation, and we
    -- can write 'updateWhere' and 'deleteWhere' to modify based on filters.
    , selectList
    , selectKeys
    , PersistQuery
    , PersistQueryRead (..)
    , PersistQueryWrite (..)
    , selectSource
    , selectKeysList

    -- * DeleteCascade
    , DeleteCascade (..)
    , deleteCascadeWhere

    -- * PersistEntity
    , PersistEntity (..)
    , SymbolToField (..)
    -- * PersistField
    , PersistField (..)
    -- * PersistConfig
    , PersistConfig (..)
    , entityValues

    -- * Lifting
    , HasPersistBackend (..)
    , withBaseBackend
    , IsPersistBackend ()
    , liftPersist
    , BackendCompatible (..)
    , withCompatibleBackend

    -- * PersistCore
    -- | 'PersistCore' is a type class that defines a default database
    -- 'BackendKey' type. For SQL databases, this is currently an
    -- auto-incrementing inteer primary key. For MongoDB, it is the default
    -- ObjectID.
    , PersistCore (..)
    , ToBackendKey (..)
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
