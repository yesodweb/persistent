-- | This module provides the tools for defining your database schema and using
-- it to generate Haskell data types and migrations.
--
-- For documentation on the domain specific language used for defining database
-- models, see "Database.Persist.Quasi".
module Database.Persist.TH
    ( -- * Parse entity defs
      persistWith
    , persistUpperCase
    , persistLowerCase
    , persistFileWith
    , persistManyFileWith

      -- * Turn @EntityDef@s into types
    , mkPersist
    , mkPersistWith

      -- ** Configuring Entity Definition
    , MkPersistSettings
    , mkPersistSettings
    , sqlSettings

      -- *** Record Fields (for update/viewing settings)
    , mpsBackend
    , mpsGeneric
    , mpsPrefixFields
    , mpsFieldLabelModifier
    , mpsAvoidHsKeyword
    , mpsConstraintLabelModifier
    , mpsEntityHaddocks
    , mpsEntityJSON
    , mpsGenerateLenses
    , mpsDeriveInstances
    , mpsCamelCaseCompositeKeySelector
    , EntityJSON (..)

      -- ** Implicit ID Columns
    , ImplicitIdDef
    , setImplicitIdDef

      -- * Various other TH functions
    , mkMigrate
    , migrateModels
    , discoverEntities
    , mkEntityDefList
    , share
    , derivePersistField
    , derivePersistFieldJSON
    , persistFieldFromEntity
    ) where

import Database.Persist.TH.Internal
